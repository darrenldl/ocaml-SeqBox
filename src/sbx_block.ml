open Stdint
open Crcccitt
open Angstrom
open Sbx_version
open Error

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

  type id = [
      `FNM
    | `SNM
    | `FSZ
    | `FDT
    | `SDT
    | `HSH
    | `PID
  ]

  let id_to_string (id:id) : string =
    match id with
    | `FNM -> "FNM"
    | `SNM -> "SNM"
    | `FSZ -> "FSZ"
    | `FDT -> "FDT"
    | `SDT -> "SDT"
    | `HSH -> "HSH"
    | `PID -> "PID"
  ;;

  let to_bytes (entry:t) : bytes =
    match entry with
    | FNM v | SNM v         -> Conv_utils.string_to_bytes v
    | FSZ v | FDT v | SDT v -> Conv_utils.uint64_to_bytes v
    | HSH v | PID v         -> Conv_utils.string_to_bytes v
  ;;

  let to_id_and_bytes (entry:t) : id * bytes =
    let res_bytes = to_bytes entry in
    match entry with
    | FNM _ -> (`FNM, res_bytes)
    | SNM _ -> (`SNM, res_bytes)
    | FSZ _ -> (`FSZ, res_bytes)
    | FDT _ -> (`FDT, res_bytes)
    | SDT _ -> (`SDT, res_bytes)
    | HSH _ -> (`HSH, res_bytes)
    | PID _ -> (`PID, res_bytes)
  ;;

  let list_to_bytes ~(ver:version) ~(fields:metadata list) : (bytes, error) result =
    let max_data_size = ver_to_data_size ver in
    let id_bytes_list = List.map to_id_and_bytes fields in
    let bytes_list    = List.map (fun (_, v) -> v) id_bytes_list in
    let all_bytes     = Bytes.concat (Bytes.create 0) bytes_list in
    let all_bytes_len = Bytes.length all_bytes in
    if      all_bytes_len < max_data_size then
      Ok Misc_utils.pad_bytes all_bytes max_data_size
    else if all_bytes_len = max_data_size then
      Ok all_bytes
    else
      Error (Bytes_too_long (List.map (fun (id, v) -> (id, Bytes.count v)) id_and_bytes_list))
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
