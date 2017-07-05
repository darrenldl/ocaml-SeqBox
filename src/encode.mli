open Sbx_version
open Sbx_block
open Stream_file

exception File_metadata_get_failed

type stats = { blocks_written : int64
             }

module Processor : sig
  (* Resulting encoder may do Printf.printf to report progress etc *)
  val make_in_out_encoder : common:Header.common_fields -> metadata:(Metadata.t list) option -> stats Stream.in_out_processor
end

module Process : sig
  val encode_file : uid:bytes option -> want_meta:bool -> in_filename:string -> out_filename:string -> (stats, string) result
end
