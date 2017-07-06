exception Length_mismatch

type hash = [ `SHA256 ]

val raw_hash_to_multihash : hash_type:hash -> raw:bytes -> bytes

val make_dummy_multihash  : hash_type:hash -> bytes
