open Sbx_block
open Sbx_specs
open Stream_file

let patch_block_bytes_if_needed (in_file:Core.In_channel.t) ~(raw_header:Header.raw_header) ~(chunk:bytes) : bytes =
  let ideal_len   = ver_to_block_size raw_header.version in
  let missing_len = ideal_len - (Bytes.length chunk) in
  if missing_len > 0 then
    match Read_chunk.read in_file ~len:missing_len with
    | None                           -> chunk (* can't do anything, just give back the original piece *)
    | Some { chunk = missing_chunk } -> Bytes.concat "" [chunk; missing_chunk]
  else
    chunk
;;

