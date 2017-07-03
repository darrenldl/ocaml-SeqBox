open Stdint

exception Invalid_range

val pad_bytes : ?filler:uint8 -> bytes -> int -> bytes

val get_bytes : bytes -> pos:int -> len:int -> bytes
