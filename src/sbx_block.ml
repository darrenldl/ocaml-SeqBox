open Stdint
open Crcccitt
open Angstrom
open Sbx_version

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

  let make_common_fields ?(uid:bytes option) ~(ver:version) : (common_fields, string) result =
    let uid:(bytes, string) result = match uid with
      | Some x ->
        let len = ver_to_file_uid_len ver in
        if Bytes.length x == len then
          Ok x
        else
          Error "length of provided uid does not match specification"
      | None   -> Ok (gen_file_uid ~ver) in
    match uid with
    | Ok uid    -> Ok { signature = ver_to_signature ver
                      ; version   = ver
                      ; file_uid  = uid }
    | Error msg -> Error msg
  ;;

  let make_metadata_header ~(common:common_fields) : t =
    { common
    ; seq_num = Some (Uint32.of_int 0)
    }
  ;;

  let make_data_header ~(common:common_fields) : t =
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

  type id =
      FNM
    | SNM
    | FSZ
    | FDT
    | SDT
    | HSH
    | PID

  let id_to_string (id:id) : string =
    match id with
    | FNM -> "FNM"
    | SNM -> "SNM"
    | FSZ -> "FSZ"
    | FDT -> "FDT"
    | SDT -> "SDT"
    | HSH -> "HSH"
    | PID -> "PID"
  ;;

  let length_distribution (lst:(id * bytes) list) : string =
    let rec length_distribution_helper (lst:(id * bytes) list) (acc:string list) : string =
      match lst with
      | []              -> (String.concat "" (List.rev acc))
      | (id, data) :: vs -> let str = Printf.sprintf "id : %s, len : %d\n" (id_to_string id) (Bytes.length data) in
        length_distribution_helper vs (str :: acc) in
    let distribution_str = length_distribution_helper lst [] in
    String.concat "" ["the length distribution of the metadata:\n"; distribution_str]
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
    | FNM _ -> (FNM, res_bytes)
    | SNM _ -> (SNM, res_bytes)
    | FSZ _ -> (FSZ, res_bytes)
    | FDT _ -> (FDT, res_bytes)
    | SDT _ -> (SDT, res_bytes)
    | HSH _ -> (HSH, res_bytes)
    | PID _ -> (PID, res_bytes)
  ;;

  let id_and_bytes_to_bytes (entry:id * bytes) : bytes =
    let (id, data) = entry in
    let id_str     = id_to_string id in
    let len        = Uint8.of_int (Bytes.length data) in
    let len_bytes  = (Bytes.create 1) in
    Uint8.to_bytes_big_endian len len_bytes 0;
    Bytes.concat (Bytes.create 0) [id_str; len_bytes; data]
  ;;

  let list_to_bytes ~(ver:version) ~(fields:t list) : (bytes, string) result =
    let max_data_size = ver_to_data_size ver in
    let id_bytes_list = List.map to_id_and_bytes fields in
    let bytes_list    = List.map id_and_bytes_to_bytes id_bytes_list in
    let all_bytes     = Bytes.concat (Bytes.create 0) bytes_list in
    let all_bytes_len = Bytes.length all_bytes in
    if      all_bytes_len < max_data_size then
      Ok (Misc_utils.pad_bytes all_bytes max_data_size)
    else if all_bytes_len = max_data_size then
      Ok all_bytes
    else
      Error (Printf.sprintf "metadata is too long when converted to bytes:\n%s" (length_distribution id_bytes_list))
  ;;
end

module Block = struct
  type t =
      Data of { header : Header.t
              ; data   : bytes }
    | Meta of { header : Header.t
              ; data   : Metadata.t list }

  let make_metadata_block ~(common:Header.common_fields) ~(fields:Metadata.t list) : t =
    Meta { header  = Header.make_metadata_header ~common
         ; data    = fields
         }
  ;;

  let make_data_block ~(common:Header.common_fields) ~(data:bytes) : t =
    Data { header = Header.make_data_header ~common
         ; data
         }
  ;;
end

type header        = Header.t

type header_common = Header.common_fields

type block         = Block.t

type metadata      = Metadata.t
