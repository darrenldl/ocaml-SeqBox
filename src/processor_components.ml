open Sbx_block

let bytes_to_block (raw_header:Header.raw_header) (chunk:bytes) : Block.t option =
  try
    Some (Block.of_bytes ~raw_header chunk)
  with
  | Header.Invalid_bytes
  | Metadata.Invalid_bytes
  | Block.Invalid_bytes
  | Block.Invalid_size     -> None
;;
