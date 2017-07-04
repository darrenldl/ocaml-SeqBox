open Sbx_version
open Stdint

module Header : sig
  exception Invalid_uid_length
  exception Missing_alt_seq_num
  exception Invalid_bytes

  type t

  type common_fields

  type raw_header =
    { version   : version
    ; crc_ccitt : uint16
    ; file_uid  : bytes
    ; seq_num   : uint32
    }

  val common_fields_to_ver : common_fields -> version

  val make_common_fields   : ?uid:bytes -> version -> common_fields

  val of_bytes             : bytes -> raw_header
end

module Metadata : sig
  exception Too_much_data of string
  exception Invalid_bytes

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
  exception Invalid_bytes

  type t

  val make_metadata_block : common:Header.common_fields -> fields:(Metadata.t list) -> t

  val make_data_block     : common:Header.common_fields -> data:bytes -> t

  val to_bytes            : ?alt_seq_num:uint32 -> t -> bytes

  val of_bytes            : ?raw_header:Header.raw_header -> ?skipped_already:bool -> bytes -> t

  val block_to_ver        : t -> version

  val block_to_file_uid   : t -> bytes

  val block_to_seq_num    : t -> uint32 option

  val block_to_data       : t -> bytes
end

(*type header        = Header.t

type header_common = Header.common_fields

type block         = Block.t

type metadata      = Metadata.t*)
