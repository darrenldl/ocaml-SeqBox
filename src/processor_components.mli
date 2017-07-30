open Sbx_block
open Sbx_specs

val try_get_block_from_in_channel           : in_channel -> int64 * (Block.t option)

val try_get_block_and_bytes_from_in_channel : in_channel -> int64 * ((Block.t * bytes) option)

val try_get_fixed_ver_block_from_in_channel           : ver:version -> in_channel -> int64 * (Block.t option)

val try_get_fixed_ver_block_and_bytes_from_in_channel : ver:version -> in_channel -> int64 * ((Block.t * bytes) option)
