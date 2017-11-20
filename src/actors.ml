let gen_file_reader
    ~(filename   : string)
    ~(chunk_size : int)
    ~(out_queue  : string option Lwt_queue.t)
  : unit -> (unit, string) result Lwt.t =
  (fun () ->
     try%lwt
       let%lwt file =
         Lwt_io.open_file ~mode:Lwt_io.Input filename in
       let rec read_loop () : (unit, string) result Lwt.t =
         let%lwt chunk = Lwt_io.read ~count:chunk_size file in
         if chunk = "" then (
           Lwt_queue.put out_queue None >>
           Lwt.return_ok ()
         )
         else (
           Lwt_queue.put out_queue None
           >>
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
       Lwt_queue.put out_queue None >> Lwt.return_ok ()
     | Unix.Unix_error (e, _, _) ->
       Lwt_queue.put out_queue None >>
       Lwt.return_error (Printf.sprintf
                           "Error when reading file : %s - %s"
                           filename (Unix.error_message e))
  )
;;

let gen_file_writer
    ~(filename : string)
    ~(in_queue : (int64 option * string) option Lwt_queue.t)
  : unit -> (unit, string) result Lwt.t =
  (fun () ->
     try%lwt
       let%lwt file =
         Lwt_io.open_file ~mode:Lwt_io.output filename in
       let rec write_loop () : (unit, string) result Lwt.t =
         match%lwt Lwt_queue.take in_queue with
         | Some (pos, data) ->
           begin
             match pos with
             | Some x -> Lwt_io.set_position file x
             | None   -> Lwt.return_unit
           end >>
           Lwt_io.write file data >>
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

let read_test () : unit Lwt.t =
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
