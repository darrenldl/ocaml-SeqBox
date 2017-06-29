type header        = Header.t

type header_common = Header.common

type block         = Block.t

module Header : sig
  type t

  type common_fields

  val make_common_fields   : ?uid:(bytes option) -> ver:version -> common_fields result

  val make_metadata_header : ver:version -> common:common_fields -> t

  val make_data_header     : ver:version -> common:common_fields -> t
end

module Metadata : sig
end

module Block : sig
  type t
  (* chunk_to_block : Header.common -> *)
end

type common_header = Header.common_header

exception Length_mismatch of string;

(* Only version 1 is supported as of time of writing *)
type version = [ `One ]

type block_type = [ `Meta | `Data | `Last_data ]

type res = (t, string) result

