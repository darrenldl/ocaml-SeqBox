open Stdint

exception Invalid_range

val pad_bytes : ?filler:uint8 -> bytes -> int -> bytes

val get_bytes : bytes -> pos:int -> len:int -> bytes

val get_bytes_inc_range : bytes -> start_at:int -> end_at:int     -> bytes

val get_bytes_exc_range : bytes -> start_at:int -> end_before:int -> bytes
