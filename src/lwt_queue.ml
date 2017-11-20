type 'a t =
  {         in_cond   : unit Lwt_condition.t
  ;         out_cond  : unit Lwt_condition.t
  ;         lock      : Lwt_mutex.t
  ;         buffer    : 'a array
  ;         dummy_val : 'a
  ;         size      : int
  ;         max       : int
  ; mutable read_pos  : int
  ; mutable write_pos : int
  }

let create ~(init_val : 'a) ~(size : int) : 'a t =
  if size <= 0 then
    raise (Invalid_argument "Size cannot be <= 0")
  else
    { in_cond   = Lwt_condition.create ()
    ; out_cond  = Lwt_condition.create ()
    ; lock      = Lwt_mutex.create ()
    ; buffer    = Array.make (size + 1) init_val
    ; dummy_val = init_val
    ; size      = size + 1
    ; max       = size
    ; read_pos  = 0
    ; write_pos = 0
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

let rec put (queue : 'a t) (v : 'a) : unit Lwt.t =
  Lwt_mutex.lock queue.lock >>

  if member_count queue = queue.max then (
    (* full, try again later *)
    Lwt_mutex.unlock queue.lock;
    Lwt_condition.wait queue.in_cond >>
    put queue v
  )
  else (
    (* has space *)
    queue.buffer.(queue.write_pos) <- v;
    queue.write_pos <- queue.write_pos ++| queue.size;
    (* singal threads waiting to take elements *)
    Lwt_condition.signal queue.out_cond ();
    Lwt_mutex.unlock queue.lock;
    Lwt.return_unit
  )
;;

let rec take (queue : 'a t) : 'a Lwt.t =
  Lwt_mutex.lock queue.lock >>

  if member_count queue = 0 then (
    (* empty, try again later *)
    Lwt_mutex.unlock queue.lock;
    Lwt_condition.wait queue.out_cond >>
    take queue
  )
  else (
    let res = queue.buffer.(queue.read_pos) in
    (* the following line is just to allow GC to collect the member *)
    queue.buffer.(queue.read_pos) <- queue.dummy_val;
    queue.read_pos <- queue.read_pos ++| queue.size;
    (* signal threads waiting to put elements *)
    Lwt_condition.signal queue.in_cond ();
    Lwt_mutex.unlock queue.lock;
    Lwt.return res
  )
;;

let test () : unit Lwt.t =
  let queue = create ~init_val:None ~size:1 in
  print_endline "test flag 1";
  let rec work1 () : unit Lwt.t =
    match%lwt take queue with
    | None   ->
      let%lwt () = Lwt_io.printlf "Done" in Lwt.return_unit
    | Some x ->
      let%lwt () = Lwt_io.printlf "Got %s" x in
      work1 () in
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
