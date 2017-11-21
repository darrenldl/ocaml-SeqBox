type 'a t =
  {         put_cond  : unit Lwt_condition.t
  ;         take_cond : unit Lwt_condition.t
  ;         lock      : Lwt_mutex.t
  ;         buffer    : 'a array
  ;         dummy_val : 'a
  ;         size      : int
  ;         max       : int
  ;         overwrite : bool
  ; mutable read_pos  : int
  ; mutable write_pos : int
  ; mutable enabled   : bool
  ; mutable serve_val : 'a option
  }

let create ?(overwrite : bool = false) ~(init_val : 'a) (size : int) : 'a t =
  if size <= 0 then
    raise (Invalid_argument "Size cannot be <= 0")
  else
    { put_cond  = Lwt_condition.create ()
    ; take_cond = Lwt_condition.create ()
    ; lock      = Lwt_mutex.create ()
    ; buffer    = Array.make (size + 1) init_val
    ; dummy_val = init_val
    ; size      = size + 1
    ; max       = size
    ; overwrite
    ; read_pos  = 0
    ; write_pos = 0
    ; enabled   = true
    ; serve_val = None
    }
;;

let (++|) (x : int) (base : int) : int =
  if x = base - 1 then 0
  else                 x + 1
;;

let member_count (queue : 'a t) : int =
  let size = queue.size in
  (* Since |write_pos - read_pos| < size, we denote this as (1)
   * if write_pos - read_pos >= 0 then
   *     (write_pos - read_pos + size) mod size
   *   = (write_pos - read_pos)        mod size
   * else write_pos - read_pos < 0
   *   from (1), we have write_pos - read_pos > - size
   *   then, we have write_pos - read_pos + size > 0,
   *   which is well suited for mod(as it's not modulo arithmetic,
   *   and thus falls apart with negative value in this use case)
   *     (write_pos - read_pos + size) mod size
   *   = (write_pos - read_pos)        mod size
  *)
  (queue.write_pos - queue.read_pos + size) mod size
;;

let write (queue : 'a t) (v : 'a) : unit =
  queue.buffer.(queue.write_pos) <- v;
  queue.write_pos <- queue.write_pos ++| queue.size
;;

let read (queue : 'a t) : 'a =
  let res = queue.buffer.(queue.read_pos) in
  (* the following line is just to allow GC to collect the member *)
  queue.buffer.(queue.read_pos) <- queue.dummy_val;
  queue.read_pos <- queue.read_pos ++| queue.size;
  res
;;

let lock (queue : 'a t) : unit Lwt.t =
  Lwt_mutex.lock queue.lock

let unlock (queue : 'a t) : unit =
  Lwt_mutex.unlock queue.lock

let signal_single_putter (queue : 'a t) : unit =
  Lwt_condition.signal queue.put_cond ()

let signal_single_taker (queue : 'a t) : unit =
  Lwt_condition.signal queue.take_cond ()

let signal_all_putters (queue : 'a t) : unit =
  Lwt_condition.broadcast queue.put_cond ()

let signal_all_takers (queue : 'a t) : unit =
  Lwt_condition.broadcast queue.take_cond ()

let wait_to_put (queue : 'a t) : unit Lwt.t =
  Lwt_condition.wait queue.put_cond

let wait_to_take (queue : 'a t) : unit Lwt.t =
  Lwt_condition.wait queue.take_cond

let write_and_unlock_and_signal ~(overwrite : bool) (queue : 'a t) (v : 'a) : unit =
  write queue v;
  begin
    if overwrite then read queue |> ignore
    else ()
  end;
  unlock queue;
  signal_single_taker queue
;;

let read_and_unlock_and_signal (queue : 'a t) : 'a =
  let res = read queue in
  unlock queue;
  (* signal threads waiting to put elements *)
  signal_single_putter queue;
  res
;;

(* internal use only *)
let is_full (queue : 'a t) : bool =
  member_count queue = queue.max

(* internal use only *)
let is_empty (queue : 'a t) : bool =
  member_count queue = 0

let is_enabled (queue : 'a t) : bool Lwt.t =
 lock queue >> Lwt.return queue.enabled

let is_disabled (queue : 'a t) : bool Lwt.t =
 lock queue >> Lwt.return (not queue.enabled)

let serve_dummy_value (queue : 'a t) : 'a =
  match queue.serve_val with
  | None   -> queue.dummy_val
  | Some x -> x
;;

let rec put (queue : 'a t) (v : 'a) : unit Lwt.t =
  lock queue >>

  if not queue.enabled then (
    unlock queue;
    Lwt.return_unit
  )
  else (
    if is_full queue then (
      if not queue.overwrite then (
        (* full, try again later *)
        unlock queue;
        wait_to_put queue >>
        put queue v
      )
      else (
        (* overwrite and shift read_pos pointer *)
        write_and_unlock_and_signal ~overwrite:true queue v;
        Lwt.return_unit
      )
    )
    else (
      (* has space *)
      write_and_unlock_and_signal ~overwrite:false queue v;
      Lwt.return_unit
    )
  )
;;

let put_no_block (queue : 'a t) (v : 'a) : bool Lwt.t =
  lock queue >>

  if not queue.enabled then (
    unlock queue;
    Lwt.return_true
  )
  else (
    if is_full queue then (
      if not queue.overwrite then (
        unlock queue;
        Lwt.return_false
      )
      else (
        (* overwrite and shift read_pos pointer *)
        write_and_unlock_and_signal ~overwrite:true queue v;
        Lwt.return_true
      )
    )
    else (
      (* has space *)
      write_and_unlock_and_signal ~overwrite:false queue v;
      Lwt.return_true
    )
  )
;;

let rec take (queue : 'a t) : 'a Lwt.t =
  lock queue >>

  if not queue.enabled then (
    unlock queue;
    Lwt.return (serve_dummy_value queue)
  )
  else (
    if is_empty queue then (
      (* empty, try again later *)
      unlock queue;
      wait_to_take queue >>
      take queue
    )
    else (
      Lwt.return (read_and_unlock_and_signal queue)
    )
  )
;;

let take_no_block (queue : 'a t) : 'a option Lwt.t =
  lock queue >>

  if not queue.enabled then (
    unlock queue;
    Lwt.return (Some (serve_dummy_value queue))
  )
  else (
    if member_count queue = 0 then (
      unlock queue;
      Lwt.return None
    )
    else (
      Lwt.return (Some (read_and_unlock_and_signal queue))
    )
  )
;;

let clear_no_lock (queue : 'a t) : unit =
  queue.read_pos  <- 0;
  queue.write_pos <- 0;

  let dummy_val = queue.dummy_val in

  for i = 0 to pred queue.size do
    queue.buffer.(i) <- dummy_val
  done;
;;

let clear (queue : 'a t) : unit Lwt.t =
  lock queue >>

  (
    clear_no_lock queue;

    unlock queue;
    signal_all_putters queue;
    Lwt.return_unit
  )
;;

let enable (queue : 'a t) : unit Lwt.t =
  lock queue >>

  if queue.enabled then (
    unlock queue;
    Lwt.return_unit
  )
  else (
    queue.enabled   <- true;
    queue.serve_val <- None;

    unlock queue;
    Lwt.return_unit
  )
;;

let disable ?(dummy_val : 'a option) (queue : 'a t) : unit Lwt.t =
  lock queue >>

  if queue.enabled then (
    clear_no_lock queue;

    queue.enabled   <- false;
    queue.serve_val <- dummy_val;

    unlock queue;
    signal_all_takers  queue;
    signal_all_putters queue;
    Lwt.return_unit
  )
  else (
    unlock queue;
    Lwt.return_unit
  )
;;

(*let test () : unit Lwt.t =
  let queue = create ~overwrite:false ~init_val:None 1 in
  print_endline "test flag 1";
  let rec work1 () : unit Lwt.t =
    match%lwt take queue with
    | None   ->
      let%lwt () = Lwt_io.printlf "Done" in Lwt.return_unit
    | Some x ->
      let%lwt () = Lwt_io.printlf "Got %s" x in
      work1 () in
  let disabler () : unit Lwt.t =
    Lwt_unix.sleep 1.5 >> disable queue in
  let work2 () : unit Lwt.t =
    for%lwt i = 1 to 10 do
      Lwt_unix.sleep 0.5 >>
      put queue (Some (string_of_int i))
    done in
  let work3 () : unit Lwt.t =
    for%lwt i = 1 to 10 do
      Lwt_unix.sleep 1.0 >>
      put queue (Some (string_of_int i))
    done >>
    let%lwt () = put queue None in
    Lwt.return_unit in
  let work4 () : unit Lwt.t =
    let%lwt () =
      Lwt_unix.sleep 1.5 >>
      for%lwt i = 100 to 150 do
        put queue (Some (string_of_int i))
      done in
    Lwt_io.printlf "work4 done" in
  print_endline "test flag 2";
  Lwt.async disabler;
  Lwt.async work2;
  Lwt.async work3;
  Lwt.async work4;
  print_endline "test flag 3";
  let waiter1, wakener1 = Lwt.wait () in
  let worker1 = Lwt.bind waiter1 work1 in
  Lwt.wakeup wakener1 ();
  print_endline "test flag 4";
  Lwt.join [worker1]
;;

  let%lwt () = test ()
*)
