exception Length_mismatch

type hash_type  = [ `SHA1
                  | `SHA2_256     | `SHA256
                  | `SHA2_512_256
                  | `SHA2_512_512 | `SHA512
                  | `BLAKE2B_256
                  | `BLAKE2B_512
                  | `BLAKE2S_128
                  | `BLAKE2S_256
                  ]

type hash_bytes

module Specs : sig
  type param = { hash_func_type : bytes
               ; digest_length  : int
               }

  val hash_type_to_param          : hash_type:hash_type -> param

  val hash_type_to_hash_func_type : hash_type:hash_type -> bytes

  val hash_type_to_digest_length  : hash_type:hash_type -> int

  val hash_type_to_total_length   : hash_type:hash_type -> int
end

val raw_hash_to_hash_bytes  : hash_type:hash_type   -> raw:bytes -> hash_bytes

val hash_bytes_to_raw_hash  : hash_bytes:hash_bytes -> bytes

val hash_bytes_to_multihash : hash_bytes:hash_bytes -> bytes

val raw_hash_to_multihash   : hash_type:hash_type   -> raw:bytes -> bytes

val make_dummy_hash_bytes   : hash_type:hash_type   -> hash_bytes

module Parser : sig
  val gen_parser : hash_type:hash_type -> hash_bytes Angstrom.t
end
