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

  let make_common_fields ?(uid:bytes option) (ver:version) : common_fields result =
    let uid = match uid with
      | Some x ->
        let len = ver_to_file_uid_len ver in
        if Bytes.length x == len then
          x
        else
          raise (Length_mismatch "length of provided UID does not match specification")
      | None   -> gen_file_uid ~ver in
    { signature = ver_to_signature ver
    ; version   = ver
    ; file_uid  = uid
    }
  ;;

  let of_bytes (raw:bytes) : individual

end

module Block = struct
  type metadata_id = FNM | SNM | FSZ | FDT | SDT | HSH | PID

  type metadata =
    { id   : meta_id
    ; len  : uint8
    ; data : bytes
    }

  type block =
      Data of { header : Header.t
              ; data   : bytes }
    | Meta of { header : Header.t
              ; data   : metadata list }

  type t = block

  let make_metadata_header ~(ver:version) ~(common:common_fields) : Header.t =
    { common
    ; seq_num = None
    }
  ;;

  let make_metadata_block ~(ver:version) ~(common:common_fields) ~(fields:metadata list) : t =
    let data = encode_metadata_list in
    { header = make_metadata_header ~ver ~com_head
