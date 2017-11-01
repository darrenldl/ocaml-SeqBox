open Stdint
open Nocrypto.Hash

type date_time_mode = [ `UTC | `Local ]

type case = [ `Upper | `Lower ]

val uint64_to_string : uint64 -> string

val uint32_to_string : uint32 -> string

val uint16_to_string : uint16 -> string

val uint8_to_string  : uint8  -> string

val bytes_to_hex_string      : case:case -> bytes  -> string

val bytes_to_hex_string_uid  : bytes     -> string

val bytes_to_hex_string_hash : bytes     -> string

val hex_string_to_bytes : string -> (bytes, string) result

val sha256_hash_state_to_bytes : SHA256.t -> bytes

val uint64_seconds_to_date_time_string : uint64 -> date_time_mode -> string
