open Sbx_block

val try_get_block_from_in_channel           : in_channel -> int64 * (Block.t option)

val try_get_block_and_bytes_from_in_channel : in_channel -> int64 * ((Block.t * bytes) option)
