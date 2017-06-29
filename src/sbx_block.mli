open Sbx_version
open Stdint

module Header : sig
  type t

  type common_fields

  val make_common_fields   : ?uid:bytes -> ver:version -> (common_fields, string) result
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

  val to_bytes      : t -> bytes

  val list_to_bytes : ver:version -> fields:(t list) -> (bytes, string) result
end

module Block : sig
  type t

  val make_metadata_block : common:Header.common_fields -> fields:(Metadata.t list) -> t

  val make_data_block     : common:Header.common_fields -> data:bytes -> t
end

type header        = Header.t

type header_common = Header.common_fields

type block         = Block.t

type metadata      = Metadata.t
