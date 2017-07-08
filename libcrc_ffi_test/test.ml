open Stdint
open Hex
open Crcccitt_wrap

let test () : unit =
  let res     = crc_ccitt_generic ~data:"abcd" ~start_val:(Uint16.of_int 0xFFFF) in
  let res_str = "  " in
  Uint16.to_bytes_big_endian res res_str 0;
  let `Hex str = Hex.of_string res_str in
  Printf.printf "got : %s\n" str
;;

test ()
