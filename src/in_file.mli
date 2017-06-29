open Sbx_version

module Raw_file : sig
  type chunks               = string list

  type multihash_and_chunks = string * chunks

  val file_hash_and_split   : ver:version -> filename:string -> (multihash_and_chunks, string) result

  val file_split            : ver:version -> filename:string -> (chunks,               string) result
end

module Sbx_file : sig

  (* val file_to_blocks        : ver:version -> filename:string -> Block.t list *)
end
