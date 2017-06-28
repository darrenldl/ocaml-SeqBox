open Sbx_version

type multihash_and_chunks  = bytes * (bytes list)

val file_hash_and_split : ver:version -> filename:string -> multihash_and_chunks
