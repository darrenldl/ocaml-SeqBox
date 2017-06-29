open Stdint

let uint64_to_bytes (v:uint64) : bytes =
  let buf = Bytes.create 8 in
  Uint64.to_bytes_big_endian v buf 0;
  buf
;;

let string_to_bytes (str:string) : bytes =
  Bytes.of_string str
;;
