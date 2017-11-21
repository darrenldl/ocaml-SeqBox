module Writer = struct
  type write_req =
    | No_position   of string
    | With_position of int64 * string
    | Set_position  of int64
    | Get_position

  type reply =
    | Position of int64

  let get_position
      ~(request_queue : write_req Lwt_queue.t)
      ~(reply_queue   : reply Lwt_queue.t)
    : int64 Lwt.t =
    let rec loop () : int64 Lwt.t =
      match%lwt Lwt_queue.take reply_queue with
      | Position pos -> Lwt.return pos
      (*| _ as x -> Lwt_queue.put reply_queue x >> loop ()*) in
    Lwt_queue.put request_queue Get_position >>
    loop ()
  ;;

  let set_position
      ~(request_queue : write_req Lwt_queue.t)
      (pos : int64)
    : unit Lwt.t =
    Lwt_queue.put request_queue (Set_position pos)
  ;;
end

let gen_file_reader
    ~(filename   : string)
    ~(chunk_size : int)
    ~(out_queue  : string option Lwt_mvar.t)
  : unit -> (unit, string) result Lwt.t =
  (fun () ->
     try%lwt
       let%lwt file =
         Lwt_io.open_file ~mode:Lwt_io.Input filename in
       let buf = Bytes.make chunk_size '\000' in
       let rec read_loop () : (unit, string) result Lwt.t =
         let%lwt read_count =
           Lwt_utils.IO.read_with_jitter ~buf file ~len:chunk_size in
         if read_count = 0 then (
           Lwt_mvar.put out_queue None >>
           Lwt.return_ok ()
         )
         else (
           let chunk = Bytes.sub_string buf 0 read_count in
           Lwt_mvar.put out_queue (Some chunk) >>
           read_loop ()
         ) in
       Protect.lwt_protect
         ~f:read_loop
         ~finally:(fun () ->
             try%lwt
               Lwt_io.close file
             with
             | _ -> Lwt.return_unit)
     with
     | End_of_file       ->
       Lwt_mvar.put out_queue None >> Lwt.return_ok ()
     | Unix.Unix_error (e, _, _) ->
       Lwt_mvar.put out_queue None >>
       Lwt.return_error (Printf.sprintf
                           "Error when reading file : %s - %s"
                           filename (Unix.error_message e))
  )
;;

let gen_file_writer
    ~(filename    : string)
    ~(in_queue    : Writer.write_req option Lwt_mvar.t)
    ~(reply_queue : Writer.reply Lwt_queue.t)
  : unit -> (unit, string) result Lwt.t =
  (fun () ->
     try%lwt
       let%lwt file =
         Lwt_io.open_file ~mode:Lwt_io.output filename in
       let rec write_loop () : (unit, string) result Lwt.t =
         match%lwt Lwt_mvar.take in_queue with
         | Some req ->
           (
             match req with
             | With_position (pos, data) ->
               Lwt_io.set_position file pos
             | No_position data -> Lwt.return_unit
             | Set_position pos ->
               Lwt_io.set_position file pos
             | Get_position ->
               Lwt_queue.put reply_queue (Position (Lwt_io.position file))
           ) >>
           let data =
             match req with
             | With_position (_, data) -> Some data
             | No_position data        -> Some data
             | Set_position _          -> None
             | Get_position            -> None
           in
           (
             match data with
             | Some data ->
               Lwt_io.write file data
             | None -> Lwt.return_unit
           ) >>
           write_loop ()
         | None -> Lwt.return_ok () in
       Protect.lwt_protect
         ~f:write_loop
         ~finally:(fun () ->
             try%lwt
               Lwt_io.close file
             with
             | _ -> Lwt.return_unit)
     with
     | Unix.Unix_error (e, _, _) ->
       Lwt.return_error (Printf.sprintf
                           "Error when reading file : %s - %s"
                           filename (Unix.error_message e))
  )
;;

let gen_duplicator
    ~(in_queue   : 'a Lwt_queue.t)
    ~(out_queues : 'a Lwt_queue.t list)
    ~(stop_pred  : 'a -> bool)
    ~(forward_stopper:bool)
  : (unit -> unit Lwt.t) =
  (fun () ->
     let broadcast (x:'a) : unit Lwt.t =
       Lwt_list.iter_p
         (fun queue -> Lwt_queue.put queue x) out_queues in
     let rec loop () : unit Lwt.t =
       let%lwt res = Lwt_queue.take in_queue in
       if stop_pred res then (
         if forward_stopper then broadcast res
         else                    Lwt.return_unit
       )
       else (
         broadcast res >> loop ()
       ) in
     loop ()
  )
;;

(*let read_test () : unit Lwt.t =
  let queue = Lwt_queue.create ~init_val:None 1 in
  print_endline "test flag 1";
  let read_work = gen_file_reader ~filename:"helo" ~chunk_size:1 ~out_queue:queue in
  print_endline "test flag 2";
  let waiter1,wakener1 = Lwt.wait () in
  let reader = Lwt.bind waiter1 read_work in
  print_endline "test flag 3";
  Lwt.wakeup wakener1 ();
  print_endline "test flag 4";
  begin
    match%lwt Lwt_queue.take queue with
    | Some x -> Lwt_io.printlf "Got : %S" x
    | None -> Lwt_io.printlf "Got nothing"
  end >>
  begin
    match%lwt reader with
    | Ok () -> Lwt_io.printlf "Okay"
    | Error msg -> Lwt_io.printlf "%s" msg
  end >>
  match Lwt.state reader with
  | Lwt.Return _ | Lwt.Sleep -> Lwt_io.printlf "Okay"
  | Lwt.Fail _ -> Lwt_io.printlf "Failed"
;;

let write_test () : unit Lwt.t =
  let queue = Lwt_queue.create ~init_val:None 1 in
  print_endline "test flag 1";
  let write_work = gen_file_writer ~filename:"hello2" ~in_queue:queue in
  print_endline "test flag 2";
  let waiter1, wakener1 = Lwt.wait () in
  let writer = Lwt.bind waiter1 write_work in
  print_endline "test flag 3";
  Lwt.wakeup wakener1 ();
  print_endline "test flag 4";
  Lwt_queue.put queue (Some (None, "hello")) >>
  Lwt_queue.put queue (Some (None, "world")) >>
  Lwt_queue.put queue None >>
  begin
    match%lwt writer with
    | Ok () -> Lwt_io.printlf "Okay"
    | Error msg -> Lwt_io.printlf "%s" msg
  end
;;

let%lwt () = read_test ()
*)
