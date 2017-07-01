open Core
open Nocrypto.Hash
open Sbx_version

let sprintf_failed_to_rw ~(in_filename:string) ~(out_filename:string) : string =
  Printf.sprintf "failed to read %s and/or failed to write %s" in_filename out_filename
;;

module Stream = struct
  let process_in_out ~(in_filename:string) ~(out_filename:string) ~(processor:(In_channel.t -> Out_channel.t -> ('a, string) result)) : ('a, string) result =
    try
      let in_file  = In_channel.create  ~binary:true in_filename  in
      protect ~f:(fun () ->
          let out_file = Out_channel.create ~binary:true out_filename in
          protect ~f:(fun () -> processor in_file out_file)
            ~finally:(fun () ->
                Out_channel.close out_file))
        ~finally:(fun () ->
            In_channel.close in_file)
    with
    | _ -> Error (sprintf_failed_to_rw ~in_filename ~out_filename)
  ;;
end

let test_copy () : unit =
  let copy_processor (in_file:In_channel.t) (out_file:Out_channel.t) : (unit, string) result =
    let read_block_size : int = 100 in
    let buf                   = String.make read_block_size '\x00' in
    let rec copy_processor_helper () =
      let read_count = In_channel.input in_file ~buf ~pos:0 ~len:read_block_size in
      Out_channel.output out_file ~buf ~pos:0 ~len:read_count;
      if read_count < read_block_size then
        Ok ()
      else
        copy_processor_helper () in
    copy_processor_helper () in
  match Stream.process_in_out ~in_filename:"dummy_file" ~out_filename:"dummy_file_copy" ~processor:copy_processor with
  | Ok _      -> Printf.printf "Okay\n"
  | Error msg -> Printf.printf "Error : %s\n" msg
;;

test_copy ()
