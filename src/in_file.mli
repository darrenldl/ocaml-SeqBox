(* All functions within In_file module put all data in memory
 *  Pros
 *    - good if you need in memory processing of all data
 *  Cons
 *    - bad if you need to process larger than memory files(will probably crash)
 *)
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

module Encode : sig
end
