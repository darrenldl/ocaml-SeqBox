open Stdint

let () =
  for _ = 0 to 1_000_000_000 do
    let input    = Random_utils.gen_bytes ~len:100 in
    let res_ffi  = Conv_utils.uint16_to_bytes (Crcccitt_wrap.crc_ccitt_generic ~input ~start_val:(Uint16.of_int 0)) in
    let res_pure = Conv_utils.uint16_to_bytes (Crcccitt.crc_ccitt_generic_uint16 ~input ~start_val:(Uint16.of_int 0)) in
    if (Bytes.compare res_ffi res_pure) = 0 then
      ()
    else
      begin
        Printf.printf "Mismatch detected :\n";
        Printf.printf "  ffi  : %s\n" (Conv_utils.bytes_to_hex_string res_ffi);
        Printf.printf "  pure : %s\n" (Conv_utils.bytes_to_hex_string res_pure)
      end
  done
;;
