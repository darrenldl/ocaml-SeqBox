open Sbx_version

module Raw_file : sig
  type multihash_and_chunks = string * (string list)

  val file_hash_and_split   : ver:version -> filename:string -> (multihash_and_chunks, string) result
end

module Sbx_file : sig
  type chunks               = string list

  val file_split            : ver:version -> filename:string -> (chunks,               string) result
end
