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
      let out_file = Out_channel.create ~binary:true out_filename in
      protect ~f:(fun () -> processor in_file out_file)
        ~finally:(fun () ->
            In_channel.close  in_file;
            Out_channel.close out_file)
    with
    | _ -> Error (sprintf_failed_to_rw ~in_filename ~out_filename)
  ;;
end
