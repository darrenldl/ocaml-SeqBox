exception Length_mismatch

type hash_type = [ `SHA256 ]

type hash_bytes

val raw_hash_to_hash_bytes  : hash_type:hash_type   -> raw:bytes -> hash_bytes

val hash_bytes_to_raw_hash  : hash_bytes:hash_bytes -> bytes

val hash_bytes_to_multihash : hash_bytes:hash_bytes -> bytes

val raw_hash_to_multihash   : hash_type:hash_type   -> raw:bytes -> bytes

val make_dummy_hash_bytes   : hash_type:hash_type   -> hash_bytes

module Parser : sig
  val sha256_p : hash_bytes Angstrom.t
end
