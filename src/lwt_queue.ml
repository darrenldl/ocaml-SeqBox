type 'a t =
  {         in_cond   : unit Lwt_condition.t
  ;         out_cond  : unit Lwt_condition.t
  ;         lock      : Lwt_mutex.t
  ;         buffer    : 'a array
  ;         size      : int
  ;         max       : int
  ; mutable read_pos  : int
  ; mutable write_pos : int
  }

let create_empty ~(init_val : 'a) ~(size : int) : 'a t =
  { in_cond   = Lwt_condition.create ()
  ; out_cond  = Lwt_condition.create ()
  ; lock      = Lwt_mutex.create ()
  ; buffer    = Array.make size init_val
  ; size
  ; max       = size - 1
  ; read_pos  = 0
  ; write_pos = 0
  }


let (++|) (x : int) (base : int) : int =
  if x = base - 1 then 0
  else                 x + 1
;;

let member_count (queue : 'a t) : int =
  let size = queue.size in
  (* Since |write_pos - read_pos| < size (1)
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
  let%lwt () = Lwt_mutex.lock queue.lock in

  if member_count queue = queue.max then (
    (* full, try again later *)
    Lwt_mutex.unlock queue.lock;
    let%lwt () = Lwt_condition.wait queue.in_cond in
    put queue v
  )
  else (
    (* has space *)
    queue.buffer.(queue.write_pos) <- v;
    queue.write_pos <- queue.write_pos ++| queue.size;
    (* singal threads waiting to take elements *)
    Lwt_condition.signal queue.out_cond ();
    Lwt_mutex.unlock queue.lock;
    Lwt.return ()
  )
;;

let rec take (queue : 'a t) : 'a Lwt.t =
  let%lwt () = Lwt_mutex.lock queue.lock in

  if member_count queue = 0 then (
    (* empty, try again later *)
    Lwt_mutex.unlock queue.lock;
    let%lwt () = Lwt_condition.wait queue.out_cond in
    take queue
  )
  else (
    let res = queue.buffer.(queue.read_pos) in
    queue.read_pos <- queue.read_pos ++| queue.size;
    (* signal threads waiting to put elements *)
    Lwt_condition.signal queue.in_cond ();
    Lwt_mutex.unlock queue.lock;
    Lwt.return res
  )
;;

let create ~(init_val : 'a) ~(size : int) (v : 'a) : 'a t =
  let res = create_empty ~init_val ~size in
  put res v |> Lwt.ignore_result;
  res
;;

let test () : unit =
  let queue = create_empty ~init_val:None ~size:4 in
  print_endline "test flag 1";
  let rec work1 () : unit Lwt.t =
    match%lwt take queue with
    | None   -> Lwt_io.printlf "Done" |>Lwt.ignore_result; Lwt.return ()
    | Some x ->
      Lwt_io.printlf "got %s" x |> Lwt.ignore_result;
      work1 () in
  print_endline "test flag 2";
  Lwt.async work1;
  print_endline "test flag 3";
  for i = 1 to 10 do
    put queue (Some (string_of_int i)) |> Lwt.ignore_result
  done;
  put queue None |> Lwt.ignore_result
;;

let () = test ()
