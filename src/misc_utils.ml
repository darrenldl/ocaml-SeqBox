open Stdint

let pad_bytes (old_bytes:bytes) (new_len:int) : bytes =
  let old_len = Bytes.length old_bytes in
  if old_len < new_len then
    let new_bytes = Bytes.make new_len '\x00' in
    Bytes.blit old_bytes 0 new_bytes 0 old_len;
    new_bytes
  else
    old_bytes
;;
