open Stdint
open Crcccitt
open Angstrom
open Sbx_version

let pad_header_or_block_bytes (old_bytes:bytes) (new_len:int) : bytes =
  Misc_utils.pad_bytes ~filler:(Uint8.of_int 0x1a) old_bytes new_len
;;

module Header = struct
  exception Invalid_uid_length
  exception Missing_alt_seq_num

  type common_fields =
    { signature  : bytes
    ; version    : version
    ; file_uid   : bytes
    }

  type t =
    { common     : common_fields
    ; seq_num    : uint32 option
    }

  let common_fields_to_ver (common:common_fields) : version =
    common.version
  ;;

  let gen_file_uid ~(ver:version) : bytes =
    let len = ver_to_file_uid_len ver in
    Random_utils.gen_bytes ~len
  ;;

  let make_common_fields ?(uid:bytes option) (ver:version) : common_fields =
    let uid : bytes = match uid with
      | Some x ->
        let len = ver_to_file_uid_len ver in
        if Bytes.length x == len then
          x
        else
          raise Invalid_uid_length
      | None   -> gen_file_uid ~ver in
    { signature = ver_to_signature ver
    ; version   = ver
    ; file_uid  = uid }
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

  let crc_ccitt_sbx ~(ver:version) ~(input:bytes) : bytes =
    let res = crc_ccitt_generic ~input ~start_val:(ver_to_uint16 ver) in
    Conv_utils.uint16_to_bytes res
  ;;

  let to_bytes ~(alt_seq_num:uint32 option) ~(header:t) ~(data:bytes) : bytes =
    let seq_num =
      match (alt_seq_num, header.seq_num) with
      | (Some s, Some _) -> Some s    (* prefer provided number over existing one *)
      | (None,   Some s) -> Some s
      | (Some s, None)   -> Some s
      | (None,   None)   -> None   in
    match seq_num with
    | Some seq_num ->
      let seq_num_bytes : bytes      = Conv_utils.uint32_to_bytes seq_num in
      let things_to_crc : bytes list = [ header.common.file_uid
                                       ; seq_num_bytes
                                       ; data
                                       ] in
      let bytes_to_crc  : bytes      = Bytes.concat "" things_to_crc in
      let crc_result    : bytes      = crc_ccitt_sbx ~ver:header.common.version ~input:bytes_to_crc in
      let header_parts  : bytes list = [ header.common.signature
                                       ; Conv_utils.uint8_to_bytes (ver_to_uint8 header.common.version)
                                       ; crc_result
                                       ; header.common.file_uid
                                       ; seq_num_bytes
                                       ] in
      Bytes.concat "" header_parts
    | None ->
      raise Missing_alt_seq_num
  ;;
end

module Metadata = struct
  exception Too_much_data of string

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
      | []              -> (String.concat "\n" (List.rev acc))
      | (id, data) :: vs -> let str = Printf.sprintf "id : %s, len : %d" (id_to_string id) (Bytes.length data) in
        length_distribution_helper vs (str :: acc) in
    let distribution_str = length_distribution_helper lst [] in
    String.concat "\n" ["the length distribution of the metadata:"; distribution_str]
  ;;

  let to_bytes (entry:t) : bytes =
    match entry with
    | FNM v | SNM v         -> Conv_utils.string_to_bytes v
    | FSZ v | FDT v | SDT v -> Conv_utils.uint64_to_bytes v
    | HSH v | PID v         ->                            v
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
    let len_bytes  = Conv_utils.uint8_to_bytes len in
    Bytes.concat (Bytes.create 0) [id_str; len_bytes; data]
  ;;

  let list_to_bytes ~(ver:version) ~(fields:t list) : bytes =
    let max_data_size = ver_to_data_size ver in
    let id_bytes_list = List.map to_id_and_bytes fields in
    let bytes_list    = List.map id_and_bytes_to_bytes id_bytes_list in
    let all_bytes     = Bytes.concat (Bytes.create 0) bytes_list in
    let all_bytes_len = Bytes.length all_bytes in
    if all_bytes_len <= max_data_size then
      pad_header_or_block_bytes all_bytes max_data_size
    else
      raise (Too_much_data (Printf.sprintf "metadata is too long when converted to bytes\n%s" (length_distribution id_bytes_list)))
  ;;
end

module Block = struct
  exception Too_much_data

  type t =
      Data of { header : Header.t
              ; data   : bytes }
    | Meta of { header : Header.t
              ; fields : Metadata.t list
              ; data   : bytes }

  let make_metadata_block ~(common:Header.common_fields) ~(fields:Metadata.t list) : t =
    (* encode once to make sure the size is okay *)
    let ver              = common.version in
    let encoded_metadata = Metadata.list_to_bytes ~ver ~fields in
    Meta { header = Header.make_metadata_header ~common
         ; fields
         ; data   = encoded_metadata}
  ;;

  let make_data_block ~(common:Header.common_fields) ~(data:bytes) : t =
    let ver           = common.version in
    let max_data_size = ver_to_data_size ver in
    let len           = Bytes.length data in
    if len <= max_data_size then
      Data { header = Header.make_data_header ~common
           ; data   = pad_header_or_block_bytes data max_data_size }
    else
      raise Too_much_data
  ;;

  let to_bytes ?(alt_seq_num:uint32 option) (block:t) : bytes =
    let (header, data) =
      match block with
      | Data { header; data } | Meta { header; data; _ } -> (header, data) in
    let header_bytes = Header.to_bytes ~alt_seq_num ~header ~data in
    Bytes.concat "" [header_bytes; data]
  ;;
end

type header        = Header.t

type header_common = Header.common_fields

type block         = Block.t

type metadata      = Metadata.t

(*
let test_metadata_block () : unit =
  let open Metadata in
  let fields : t list = [ FNM (String.make 10000 'a')
                        ; SNM "filename.sbx"
                        ; FSZ (Uint64.of_int 100)
                        ; FDT (Uint64.of_int 100000)
                        ; SDT (Uint64.of_int 100001)
                        ; HSH "1220edeaaff3f1774ad2888673770c6d64097e391bc362d7d6fb34982ddf0efd18cb"
                        ] in
  try
    let common = Header.make_common_fields `V1 in
    let metadata_block = Block.make_metadata_block ~common ~fields in
    let bytes = Block.make_block_bytes metadata_block in
    Printf.printf "Okay :\n%s\n" (Hex.hexdump_s (Hex.of_string bytes))
  with
  | Metadata.Too_much_data str -> print_endline str
;;

let test_data_block () : unit =
  let data = (Bytes.make 496 '\x00') in
  let common = Header.make_common_fields `V1 in
  let data_block = Block.make_data_block ~common ~data in
  let bytes = Block.make_block_bytes ~alt_seq_num:(Uint32.of_int 0) data_block in
  Printf.printf "Okay :\n%s\n" (Hex.hexdump_s (Hex.of_string bytes))
;;

test_metadata_block ();
test_data_block ()
  *)
