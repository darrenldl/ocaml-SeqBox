open Stdint

exception Invalid_range

let pad_bytes ?(filler:uint8 = Uint8.of_int 0x00) (old_bytes:bytes) (new_len:int) : bytes =
  let buf         = Bytes.create 1 in
  Uint8.to_bytes_big_endian filler buf 0;
  let filler_char = Bytes.get buf 0 in
  let old_len     = Bytes.length old_bytes in
  if old_len < new_len then
    let new_bytes = Bytes.make new_len filler_char in
    Bytes.blit old_bytes 0 new_bytes 0 old_len;
    new_bytes
  else
    old_bytes
;;

let get_bytes (chunk:bytes) ~(pos:int) ~(len:int) : bytes =
  let chunk_size = Bytes.length chunk in
  if      pos < 0 || pos >= chunk_size then
    raise Invalid_range
  else if len < 0 then
    raise Invalid_range
  else if pos + len - 1 >= chunk_size then
    raise Invalid_range
  else
    Bytes.sub chunk pos len
;;

let get_bytes_inc_range (chunk:bytes) ~(start_at:int) ~(end_at:int) : bytes =
  get_bytes chunk ~pos:start_at ~len:(end_at     - start_at + 1)
;;

let get_bytes_exc_range (chunk:bytes) ~(start_at:int) ~(end_before:int) : bytes =
  get_bytes chunk ~pos:start_at ~len:(end_before - start_at)
;;
