open Stdint

let uint64_to_bytes (v:uint64) : bytes =
  let buf = Bytes.create 8 in
  Uint64.to_bytes_big_endian v buf 0;
  buf
;;

let uint32_to_bytes (v:uint32) : bytes =
  let buf = Bytes.create 4 in
  Uint32.to_bytes_big_endian v buf 0;
  buf
;;

let uint16_to_bytes (v:uint16) : bytes =
  let buf = Bytes.create 2 in
  Uint16.to_bytes_big_endian v buf 0;
  buf
;;

let uint8_to_bytes (v:uint8) : bytes =
  let buf = Bytes.create 1 in
  Uint8.to_bytes_big_endian v buf 0;
  buf
;;

let string_to_bytes (str:string) : bytes =
  Bytes.of_string str
;;

let bytes_to_hex_string (data:bytes) : string =
  let `Hex str = Hex.of_cstruct (Cstruct.of_bytes data) in
  str
;;

