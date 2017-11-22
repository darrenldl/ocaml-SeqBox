open Sbx_block
open Stream_file
open Sbx_specs

exception File_metadata_get_failed

module Stats : sig
  type t

  val print_stats : t -> unit
end

type stats = Stats.t

module Process : sig
  val encode_file : uid:string option -> want_meta:bool -> ver:version -> hash:string -> in_filename:string -> out_filename:string -> (stats, string) result
end
