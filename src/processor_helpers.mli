open Sbx_block

val patch_block_bytes_if_needed : Core.In_channel.t -> raw_header:Header.raw_header -> chunk:bytes -> bytes
