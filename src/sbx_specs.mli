open Stdint

(* Only version 1 is supported as of time of writing *)
type version = [ `V1 ]

val sbx_file_uid_len    : int

val sbx_signature       : bytes

val sbx_header_size     : int

val ver_to_int          : version -> int

val ver_to_uint8        : version -> uint8

val ver_to_uint16       : version -> uint16

val ver_to_bytes        : version -> bytes

val ver_to_block_size   : version -> int

val ver_to_data_size    : version -> int
