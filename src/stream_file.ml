module Sprintf_helper = struct
  let sprintf_failed_to_rw    ~(in_filename:string) ~(out_filename:string) : string =
    Printf.sprintf "failed to read %s and/or failed to write %s" in_filename out_filename
  ;;

  let sprintf_failed_to_read  ~(in_filename:string)  : string =
    Printf.sprintf "failed to read %s"  in_filename
  ;;

  let sprintf_failed_to_write ~(out_filename:string) : string =
    Printf.sprintf "failed to write %s" out_filename
  ;;
end

module General_helper = struct
  exception Invalid_range

  let make_buffer (size:int) : bytes =
    Bytes.make size '\x00'
  ;;

  let get_from_buf ~(buf:bytes) ~(pos:int) ~(len:int) : bytes =
    let buf_size = Bytes.length buf in
    if      pos < 0 || pos >= buf_size then
      raise Invalid_range
    else if len <= 0 then
      raise Invalid_range
    else if pos + len >= buf_size then
      raise Invalid_range
    else
      Bytes.sub buf pos len
  ;;

  let get_from_buf_inc_range ~(buf:bytes) ~(start_at:int) ~(end_at:int) : bytes =
    get_from_buf ~buf ~pos:start_at ~len:(end_at     - start_at + 1)
  ;;

  let get_from_buf_exc_range ~(buf:bytes) ~(start_at:int) ~(end_before:int) : bytes =
    get_from_buf ~buf ~pos:start_at ~len:(end_before - start_at)
  ;;
end

module Read_into_buf = struct
  exception Invalid_offset
  exception Invalid_length

  type read_result = { no_more_bytes : bool
                     ; read_count    : int
                     }

  let read ?(offset:int = 0) ?(len:int option) (in_file:Core.In_channel.t) ~(buf:bytes) : read_result =
    let buf_size = Bytes.length buf in
    if offset >= buf_size then
      raise Invalid_offset
    else
      let len : int =
        match len with
        | Some x -> x
        | None   -> (Bytes.length buf) - offset in
      if len <= 0 then
        raise Invalid_length
      else
        let read_count    : int  = Core.In_channel.input in_file ~buf ~pos:offset ~len in
        let no_more_bytes : bool = read_count < len in
        {no_more_bytes; read_count}
  ;;
end

module Read_chunk = struct
  type read_result = { no_more_bytes : bool
                     ; chunk         : bytes
                     }

  let read (in_file:Core.In_channel.t) ~(len:int) : read_result =
    try
      let buf = General_helper.make_buffer len in
      let {no_more_bytes; read_count} : Read_into_buf.read_result = Read_into_buf.read in_file ~buf in
      let chunk = General_helper.get_from_buf ~buf ~pos:0 ~len:read_count in
      {no_more_bytes; chunk}
    with
    (* Read_chunk.read should never raise any exceptions related to use of Read_into_buf.read *)
    | Read_into_buf.Invalid_offset
    | Read_into_buf.Invalid_length -> assert false 
  ;;
end

module Write_from_buf = struct
  exception Invalid_offset
  exception Invalid_length

  let write ?(offset:int = 0) ?(len:int option) (out_file:Core.Out_channel.t) ~(buf:bytes) : unit =
    let buf_size = Bytes.length buf in
    if offset >= buf_size then
      raise Invalid_offset
    else
      let len : int =
        match len with
        | Some x -> x
        | None   -> (Bytes.length buf) - offset in
      if len <= 0 then
        raise Invalid_length
      else
        Core.Out_channel.output out_file ~buf ~pos:offset ~len
  ;;
end

module Write_chunk = struct
  let write (out_file:Core.Out_channel.t) ~(chunk:bytes) : unit =
    try
      Write_from_buf.write out_file ~buf:chunk
    with
    (* Write_chunk.write should never raise any exceptions related to use of Write_from_buf.write *)
    | Write_from_buf.Invalid_offset
    | Write_from_buf.Invalid_length -> assert false
end

module Stream = struct
  type 'a in_out_processor = Core.In_channel.t -> Core.Out_channel.t -> 'a

  type 'a in_processor     = Core.In_channel.t  -> 'a

  type 'a out_processor    = Core.Out_channel.t -> 'a

  let process_in_out ~(in_filename:string) ~(out_filename:string) ~(processor:('a in_out_processor)) : ('a, string) result =
    try
      let in_file  = Core.In_channel.create  ~binary:true in_filename  in
      let res =
        Core.protect ~f:(fun () ->
            let out_file = Core.Out_channel.create ~binary:true out_filename in
            Core.protect ~f:(fun () -> processor in_file out_file)
              ~finally:(fun () ->
                  Core.Out_channel.close out_file))
          ~finally:(fun () ->
              Core.In_channel.close in_file) in
      Ok res
    with
    | Read_into_buf.Invalid_offset  -> Error "Invalid offset provided to Read_into_buf.read"
    | Read_into_buf.Invalid_length  -> Error "Invalid length provided to Read_into_buf.read"
    | Write_from_buf.Invalid_offset -> Error "Invalid offset provided to Write_from_buf.write"
    | Write_from_buf.Invalid_length -> Error "Invalid length provided to Write_from_buf.write"
    | Sys_error _                   -> Error (Sprintf_helper.sprintf_failed_to_rw ~in_filename ~out_filename)
    | _                             -> Error "Unknown failure"
  ;;

  let process_in ~(in_filename:string) ~(processor:('a in_processor))   : ('a, string) result =
    try
      let in_file = Core.In_channel.create ~binary:true in_filename in
      let res =
        Core.protect ~f:(fun () -> processor in_file)
          ~finally:(fun () -> Core.In_channel.close in_file) in
      Ok res
    with
    | Read_into_buf.Invalid_offset -> Error "Invalid offset provided to Read_into_buf.read"
    | Read_into_buf.Invalid_length -> Error "Invalid length provided to Read_into_buf.read"
    | Sys_error _                  -> Error (Sprintf_helper.sprintf_failed_to_read ~in_filename)
    | _                            -> Error "Unknown failure"
  ;;

  let process_out ~(out_filename:string) ~(processor:('a out_processor)) : ('a, string) result =
    try
      let out_file = Core.Out_channel.create ~binary:true out_filename in
      let res =
        Core.protect ~f:(fun () -> processor out_file)
          ~finally:(fun () -> Core.Out_channel.close out_file) in
      Ok res
    with
    | Write_from_buf.Invalid_offset -> Error "Invalid offset provided to Write_from_buf.write"
    | Write_from_buf.Invalid_length -> Error "Invalid length provided to Write_from_buf.write"
    | Sys_error _                   -> Error (Sprintf_helper.sprintf_failed_to_write ~out_filename)
    | _                             -> Error "Unknown failure"
  ;;
end

(*
let test_copy () : unit =
  let open Core in
  let copy_processor (in_file:In_channel.t) (out_file:Out_channel.t) : (unit, string) result =
    let read_block_size : int = 100 in
    let buf                   = General_helper.make_buffer read_block_size in
    let rec copy_processor_helper () =
      let open Read_into_buf in
      let open Write_from_buf in
      let {no_more_bytes; read_count} = read in_file ~buf in
      write out_file ~buf ~len:read_count;
      if no_more_bytes then
        Ok ()
      else
        copy_processor_helper () in
    copy_processor_helper () in
  match Stream.process_in_out ~in_filename:"dummy_file" ~out_filename:"dummy_file_copy" ~processor:copy_processor with
  | Ok _      -> Printf.printf "Okay\n"
  | Error msg -> Printf.printf "Error : %s\n" msg
;;

test_copy ()
   *)
