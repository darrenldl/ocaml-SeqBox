open Sbx_version
open Stdint

module Header : sig
  type t

  type common_fields

  val make_common_fields   : ?uid:bytes -> version -> (common_fields, string) result
end

module Metadata : sig
  type t =
      FNM of string
    | SNM of string
    | FSZ of uint64
    | FDT of uint64
    | SDT of uint64
    | HSH of bytes
    | PID of bytes
end

module Block : sig
  type t

  val make_metadata_block : common:Header.common_fields -> fields:(Metadata.t list) -> (t, string) result

  val make_data_block     : common:Header.common_fields -> data:bytes -> (t, string) result
end

type header        = Header.t

type header_common = Header.common_fields

type block         = Block.t

type metadata      = Metadata.t
