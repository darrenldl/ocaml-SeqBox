exception Length_mismatch

type hash = [ `SHA256 ]

module Parser : sig
  val sha256_p : bytes Angstrom.t
end

val raw_hash_to_multihash : hash_type:hash -> raw:bytes -> bytes

val make_dummy_raw_hash   : hash_type:hash -> bytes
