open Stdint
open Crcccitt
open Angstrom
open Sbx_version
open Exception

type header        = Header.t

type header_common = Header.common

type block         = Block.t

module Header = struct
  type common_fields =
    { signature  : bytes
    ; version    : version
    ; file_uid   : bytes
    }

  type t =
    { common     : common_fields
    ; seq_num    : uint32 option
    }

  let gen_file_uid ~(ver:version) : bytes =
    let len = ver_to_file_uid_len ver in
    Random_utils.gen_bytes ~len
  ;;

  let make_common_fields ?(uid:bytes option) ~(ver:version) : common_fields =
    let uid = match uid with
      | Some x ->
        let len = ver_to_file_uid_len ver in
        if Bytes.length x == len then
          x
        else
          (* length of provided uid does not match specification *)
          assert false
      | None   -> gen_file_uid ~ver in
    { signature = ver_to_signature ver
    ; version   = ver
    ; file_uid  = uid
    }
  ;;

  let make_metadata_header ~(ver:version) ~(common:common_fields) : t =
    { common
    ; seq_num = Some 0
    }
  ;;

  let make_data_header ~(ver:version) ~(common:common_fields) : t =
    { common
    ; seq_num = None
    }
  ;;
end

module Metadata = struct
  type t =
      FNM of string
    | SNM of string
    | FSZ of uint64
    | FDT of uint64
    | SDT of uint64
    | HSH of bytes
    | PID of bytes

  (*let check_against_specs ~(entry:metadata) : (unit, string) result =
    match entry with
    | FNM str -> Ok ()
    | SNM str -> Ok ()
    | FSZ v   -> Ok ()
    | FDT v   -> Ok ()
    | SDT v   -> Ok ()
    | HSH hsh -> let len = (Bytes.length hsh) in
      if len == *)

  let to_bytes (entry:t) : bytes =
    match entry with
    | FNM v | SNM v         -> Conv_utils.string_to_bytes v
    | FSZ v | FDT v | SDT v -> Conv_utils.uint64_to_bytes v
    | HSH v | PID v         -> Conv_utils.string_to_bytes v
  ;;
end

module Block = struct
  type t =
      Data of { header : Header.t
              ; data   : bytes }
    | Meta of { header : Header.t
              ; data   : metadata list }

  let make_metadata_block ~(ver:version) ~(common:common_fields) ~(fields:metadata list) : (t, string) result =
    Meta of { header  = Header.make_metadata_header ~ver ~common
            ; data    = fields
            }
  ;;

  let make_data_block ~(ver:version) ~(common:common_fields) ~(data:bytes) : t =
    Data of { header = Header.make_data_header ~ver ~common
            ; data
            }
  ;;
end
