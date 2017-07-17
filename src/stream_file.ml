exception Packaged_exn of string

module Sprintf_helper = struct
  let sprintf_failed_to_rw    ~(in_filename:string) ~(out_filename:string) : string =
    Printf.sprintf "Failed to read %s and/or failed to write %s" in_filename out_filename
  ;;

  let sprintf_failed_to_read  ~(in_filename:string)  : string =
    Printf.sprintf "Failed to read %s"  in_filename
  ;;

  let sprintf_failed_to_write ~(out_filename:string) : string =
    Printf.sprintf "Failed to write %s" out_filename
  ;;
end

module General_helper = struct
  exception Invalid_range

  let make_buffer (size:int) : bytes =
    Bytes.make size '\x00'
  ;;

  let get_from_buf ~(buf:bytes) ~(pos:int) ~(len:int) : bytes =
    try
      Misc_utils.get_bytes buf ~pos ~len
    with
    | Misc_utils.Invalid_range -> raise Invalid_range
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

  type read_stats  = { read_count : int }
  type read_result = read_stats option

  let read_with_jitter ~(buf:bytes) (in_file:Core_kernel.In_channel.t) ~(len:int) : int =
    (* jitter = read where read count is smaller than requested but
     * there is still more data available in the channel
     *
     * this sometimes happen for some reason when reading large files
     * (1GiB file was being used when this occured in testing)
     *)
    let jitter_threshold = 3 in
    let rec read_with_jitter_internal ~(read_so_far:int) ~(tries_left:int) : int =
      let read_count  : int = Core_kernel.In_channel.input in_file ~buf ~pos:read_so_far ~len:(len - read_so_far) in
      let read_so_far : int = read_so_far + read_count in
      let tries_left  : int = tries_left - 1 in
      if      read_count < 0 then
        assert false
      else if read_count < len then
        if tries_left > 0 then
          read_with_jitter_internal ~read_so_far ~tries_left
        else
          read_so_far
      else
        read_so_far in
    read_with_jitter_internal ~read_so_far:0 ~tries_left:jitter_threshold
  ;;

  let read ?(offset:int = 0) ?(len:int option) (in_file:Core_kernel.In_channel.t) ~(buf:bytes) : read_result =
    let buf_size = Bytes.length buf in
    if offset >= buf_size then
      raise Invalid_offset
    else
      let len : int =
        match len with
        | Some x -> x
        | None   -> buf_size - offset in
      if len < 0 then
        raise Invalid_length
      else
        let read_count : int = read_with_jitter ~buf in_file ~len in
        if      read_count < 0 then
          assert false
        else if read_count = 0 then
          None
        else
          Some { read_count }
  ;;
end

module Read_chunk = struct
  type read_content = { chunk : bytes }
  type read_result  = read_content option

  let read (in_file:Core_kernel.In_channel.t) ~(len:int) : read_result =
    try
      let buf = General_helper.make_buffer len in
      match Read_into_buf.read in_file ~buf with
      | None                -> None
      | Some { read_count } -> let chunk = General_helper.get_from_buf ~buf ~pos:0 ~len:read_count in
        Some { chunk }
    with
    (* Read_chunk.read should never raise any exceptions related to use of Read_into_buf.read *)
    | Read_into_buf.Invalid_offset
    | Read_into_buf.Invalid_length -> assert false 
  ;;
end

module Write_from_buf = struct
  exception Invalid_offset
  exception Invalid_length

  let write ?(offset:int = 0) ?(len:int option) (out_file:Core_kernel.Out_channel.t) ~(buf:bytes) : unit =
    let buf_size = Bytes.length buf in
    if offset >= buf_size then
      raise Invalid_offset
    else
      let len : int =
        match len with
        | Some x -> x
        | None   -> buf_size - offset in
      if len < 0 then
        raise Invalid_length
      else
        Core_kernel.Out_channel.output out_file ~buf ~pos:offset ~len
  ;;
end

module Write_chunk = struct
  let write (out_file:Core_kernel.Out_channel.t) ~(chunk:bytes) : unit =
    try
      Write_from_buf.write out_file ~buf:chunk
    with
    (* Write_chunk.write should never raise any exceptions related to use of Write_from_buf.write *)
    | Write_from_buf.Invalid_offset
    | Write_from_buf.Invalid_length -> assert false
end

module Stream = struct
  type 'a in_out_processor = Core_kernel.In_channel.t  -> Core_kernel.Out_channel.t -> 'a

  type 'a in_processor     = Core_kernel.In_channel.t  -> 'a

  type 'a out_processor    = Core_kernel.Out_channel.t -> 'a

  let process_in_out ?(pack_break_into_error:bool = true) ~(append:bool) ~(in_filename:string) ~(out_filename:string) (processor:('a in_out_processor)) : ('a, string) result =
    try
      let in_file  = Core_kernel.In_channel.create ~binary:true in_filename  in
      let res =
        Core_kernel.protect ~f:(fun () ->
            let out_file = Core_kernel.Out_channel.create ~binary:true ~append out_filename in
            Core_kernel.protect ~f:(fun () -> processor in_file out_file)
              ~finally:(fun () ->
                  try
                    Core_kernel.Out_channel.close out_file
                  with
                  | _ -> () (* ignore close failures *)
                )
          )
          ~finally:(fun () ->
              try
                Core_kernel.In_channel.close in_file
              with
              | _ -> () (* ignore close failures *)
            ) in
      Ok res
    with
    | Sys.Break                       ->
      if pack_break_into_error then
        Error "Interrupted"
      else
        raise Sys.Break  (* treat break signal differently *)
    | Packaged_exn msg                -> Error msg
    | Assert_failure (loc, line, col) -> Error (Printf.sprintf "Assert failure at %s %d %d" loc line col)
    | Read_into_buf.Invalid_offset    -> Error "Invalid offset provided to Read_into_buf.read"
    | Read_into_buf.Invalid_length    -> Error "Invalid length provided to Read_into_buf.read"
    | Write_from_buf.Invalid_offset   -> Error "Invalid offset provided to Write_from_buf.write"
    | Write_from_buf.Invalid_length   -> Error "Invalid length provided to Write_from_buf.write"
    | Sys_error _                     -> Error (Sprintf_helper.sprintf_failed_to_rw ~in_filename ~out_filename)
    | _                               -> Error "Unknown failure"
  ;;

  let process_in ?(pack_break_into_error:bool = true) ~(in_filename:string) (processor:('a in_processor))   : ('a, string) result =
    try
      let in_file = Core_kernel.In_channel.create ~binary:true in_filename in
      let res =
        Core_kernel.protect ~f:(fun () -> processor in_file)
          ~finally:(fun () ->
              try
                Core_kernel.In_channel.close in_file
              with
              | _ -> () (* ignore close failures *)
            ) in
      Ok res
    with
    | Sys.Break                       ->
      if pack_break_into_error then
        Error "Interrupted"
      else
        raise Sys.Break  (* treat break signal differently *)
    | Packaged_exn msg                -> Error msg
    | Assert_failure (loc, line, col) -> Error (Printf.sprintf "Assert failure at %s %d %d" loc line col)
    | Read_into_buf.Invalid_offset    -> Error "Invalid offset provided to Read_into_buf.read"
    | Read_into_buf.Invalid_length    -> Error "Invalid length provided to Read_into_buf.read"
    | Sys_error _                     -> Error (Sprintf_helper.sprintf_failed_to_read ~in_filename)
    | _                               -> Error "Unknown failure"
  ;;

  let process_out ?(pack_break_into_error:bool = true) ~(append:bool) ~(out_filename:string) (processor:('a out_processor)) : ('a, string) result =
    try
      let out_file = Core_kernel.Out_channel.create ~binary:true ~append out_filename in
      let res =
        Core_kernel.protect ~f:(fun () -> processor out_file)
          ~finally:(fun () ->
              try
                Core_kernel.Out_channel.close out_file
              with
              | _ -> () (* ignore close failures *)
            ) in
      Ok res
    with
    | Sys.Break                       ->
      if pack_break_into_error then
        Error "Interrupted"
      else
        raise Sys.Break  (* treat break signal differently *)
    | Packaged_exn msg                -> Error msg
    | Assert_failure (loc, line, col) -> Error (Printf.sprintf "Assert failure at %s %d %d" loc line col)
    | Write_from_buf.Invalid_offset   -> Error "Invalid offset provided to Write_from_buf.write"
    | Write_from_buf.Invalid_length   -> Error "Invalid length provided to Write_from_buf.write"
    | Sys_error _                     -> Error (Sprintf_helper.sprintf_failed_to_write ~out_filename)
    | _                               -> Error "Unknown failure"
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
