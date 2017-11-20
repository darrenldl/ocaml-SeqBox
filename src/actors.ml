let gen_file_reader
    ~(filename   : string)
    ~(chunk_size : int)
    ~(out_queue  : string option Lwt_queue.t)
  : unit -> unit Lwt.t =
  (fun () ->
     let%lwt file =
       Lwt_io.open_file ~mode:Lwt_io.input filename in
     let rec read_loop () : unit Lwt.t =
       let%lwt chunk = Lwt_io.read ~count:chunk_size file in
       Lwt_queue.put out_queue (Some chunk) >>
       read_loop () in
     try%lwt
       Protect.lwt_protect
         ~f:read_loop
         ~finally:(fun () ->
             try%lwt
               Lwt_io.close file
             with
             | _ -> Lwt.return_unit)
     with
     | End_of_file -> Lwt_queue.put out_queue None
  )

let gen_file_writer
    ~(filename : string)
    ~(in_queue : (int64 * string) option Lwt_queue.t)
  : unit -> unit Lwt.t =
  (fun () ->
     let%lwt file =
       Lwt_io.open_file ~mode:Lwt_io.output filename in
     let rec write_loop () : unit Lwt.t =
       match%lwt Lwt_queue.take in_queue with
       | Some (pos, data) ->
         Lwt_io.set_position file pos >>
         Lwt_io.write file data
       | None -> Lwt.return_unit in
     Protect.lwt_protect
       ~f:write_loop
       ~finally:(fun () ->
           try%lwt
             Lwt_io.close file
           with
           | _ -> Lwt.return_unit)
  )
