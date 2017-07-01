open Nocrypto.Hash
open Sbx_version

let sprintf_failed_to_rw ~(in_filename:string) ~(out_filename:string) : string =
  Printf.sprintf "failed to read %s and/or failed to write %s" in_filename out_filename
;;

module Stream = struct
  open Core

  let process_in_out ~(in_filename:string) ~(out_filename:string) ~(processor:(Core.In_channel.t -> Core.Out_channel.t -> ('a, string) result)) : ('a, string) result =
    try
      let in_file  = Core.In_channel.create  ~binary:true in_filename  in
      protect ~f:(fun () ->
          let out_file = Core.Out_channel.create ~binary:true out_filename in
          protect ~f:(fun () -> processor in_file out_file)
            ~finally:(fun () ->
                Core.Out_channel.close out_file))
        ~finally:(fun () ->
            Core.In_channel.close in_file)
    with
    | _ -> Error (sprintf_failed_to_rw ~in_filename ~out_filename)
  ;;
end

module Helper = struct
  let read_chunk_into_buf ?(offset:int = 0) ?(len:int option) (in_file:Core.In_channel.t) ~(buf:bytes) : bool * int =
    let len : int =
      match len with
      | Some x -> x
      | None   -> (Bytes.length buf) - offset in
    let read_count    : int  = Core.In_channel.input in_file ~buf ~pos:offset ~len in
    let no_more_bytes : bool = read_count < len in
    (no_more_bytes, read_count)
  ;;

  let read_chunk (in_file:Core.In_channel.t) ~(len:int) : bool * bytes =
    let buf = Bytes.make len '\x00' in
    let (no_more_bytes, _) = read_chunk_into_buf in_file ~buf in
    (no_more_bytes, buf)
  ;;

  let write_from_buf ?(offset:int = 0) ?(len:int option) (out_file:Core.Out_channel.t) ~(buf:bytes) : unit =
    let len : int =
      match len with
      | Some x -> x
      | None   -> (Bytes.length buf) - offset in
    Core.Out_channel.output out_file ~buf ~pos:offset ~len
  ;;
end

let test_copy () : unit =
  let open Core in
  let copy_processor (in_file:In_channel.t) (out_file:Out_channel.t) : (unit, string) result =
    let read_block_size : int = 100 in
    let buf                   = String.make read_block_size '\x00' in
    let rec copy_processor_helper () =
      let (no_more_bytes, read_count) = Helper.read_chunk_into_buf in_file ~buf in
      Helper.write_from_buf out_file ~buf ~len:read_count;
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
