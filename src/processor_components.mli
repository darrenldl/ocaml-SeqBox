open Sbx_block
open Sbx_specs

val try_get_block_from_in_channel           :
  ?fixed_ver:version ->
  ?raw_header_pred:(Header.raw_header -> bool) ->
  in_channel -> int64 * (Block.t option)

val try_get_block_and_bytes_from_in_channel :
  ?fixed_ver:version ->
  ?raw_header_pred:(Header.raw_header -> bool) ->
  in_channel -> int64 * ((Block.t * string) option)
