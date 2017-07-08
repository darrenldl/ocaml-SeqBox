open Ctypes
open Foreign

let crc_ccitt_generic_ : string -> Unsigned.Size_t.t -> Unsigned.UInt16.t -> Unsigned.UInt16.t =
  (* string instead of ptr uchar should be okay since
   * this is only to transfer data to the C function
   *)
  foreign "crc_ccitt_generic" (string @-> size_t @-> uint16_t @-> returning uint16_t)
;;

let crc_ccitt_generic ~(input:bytes) ~(start_val:Stdint.uint16) =
  let input_len  : int = Bytes.length input in
  let input_size       = Unsigned.Size_t.of_int input_len in
  let start_val        = Unsigned.UInt16.of_int (Stdint.Uint16.to_int start_val) in
  let res              = crc_ccitt_generic_ input input_size start_val in
  Stdint.Uint16.of_int (Unsigned.UInt16.to_int res)
;;
