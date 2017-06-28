open Stdint

(* Only version 1 is supported as of time of writing *)
type version = [ `V1 ]

module Param_for_v1 : sig
  val file_uid_len : int
  val signature    : bytes
  val block_size   : int
end

val ver_to_int          : version -> int

val ver_to_uint16       : version -> uint16

val ver_to_file_uid_len : version -> int

val ver_to_signature    : version -> bytes
  
val ver_to_data_size    : version -> int
