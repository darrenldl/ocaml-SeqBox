open Stdint
open Nocrypto.Hash

val uint64_to_bytes : uint64 -> bytes

val uint32_to_bytes : uint32 -> bytes

val uint16_to_bytes : uint16 -> bytes

val uint8_to_bytes  : uint8  -> bytes

val string_to_bytes : string -> bytes

val bytes_to_hex_string : bytes -> string

val sha256_hash_state_to_bytes : SHA256.t -> bytes
