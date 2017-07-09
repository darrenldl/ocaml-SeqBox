module Encode = struct
let encode (in_filename:string) (out_filename:string) : unit =
  let open Encode in
  let out_filename =
    if String.length out_filename = 0 then
      String.concat "" [in_filename; ".sbx"]
    else
      out_filename in
  match encode_file ~uid:None ~want_meta:true ~in_filename ~out_filename with
  | Ok stats  -> Stats.print_stats stats
  | Error msg -> Printf.printf "Error : %s\n" msg
;;

let in_filename =
  let doc = "File to encode." in
  Arg.(value & 
end
