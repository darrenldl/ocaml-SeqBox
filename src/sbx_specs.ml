open Stdint

type version = [ `V1 | `V2 | `V3 | `V11 | `V12 | `V13 ]

module Common_param = struct
  let file_uid_len   : int    = 6
  let signature      : string = "SBx"
  let header_size    : int    = 16
  let max_blocks_num : int64  = Int64.sub (Int64.shift_left 0x1L 32) 1L  (* 2 ** 32 - 1 *)
end

module Param_for_v1 = struct
  let block_size   : int   = 512
  let data_size    : int   = block_size - Common_param.header_size
end

module Param_for_v2 = struct
  let block_size   : int   = 128
  let data_size    : int   = block_size - Common_param.header_size
end

module Param_for_v3 = struct
  let block_size   : int   = 4096
  let data_size    : int   = block_size - Common_param.header_size
end

module Param_for_v11 = struct
  let block_size   : int   = Param_for_v1.block_size
  let data_size    : int   = block_size - Common_param.header_size
end

module Param_for_v12 = struct
  let block_size   : int   = Param_for_v2.block_size
  let data_size    : int   = block_size - Common_param.header_size
end

module Param_for_v13 = struct
  let block_size   : int   = Param_for_v3.block_size
  let data_size    : int   = block_size - Common_param.header_size
end

module Parser = struct
  open Angstrom

  let v1_p  : version Angstrom.t =
    char '\001' *> return `V1
  ;;

  let v2_p  : version Angstrom.t =
    char '\002' *> return `V2
  ;;

  let v3_p  : version Angstrom.t =
    char '\003' *> return `V3
  ;;

  let v11_p : version Angstrom.t =
    char '\011' *> return `V11
  ;;

  let v12_p : version Angstrom.t =
    char '\012' *> return `V12
  ;;

  let v13_p : version Angstrom.t =
    char '\013' *> return `V13
  ;;

  let ver_p : version Angstrom.t =
    choice [ v1_p
           ; v2_p
           ; v3_p
           ; v11_p
           ; v12_p
           ; v13_p
           ]
  ;;
end

let sbx_file_uid_len = Common_param.file_uid_len;;

let sbx_signature    = Common_param.signature;;

let sbx_header_size  = Common_param.header_size;;

let ver_to_int          (ver:version) : int =
  match ver with
  | `V1  -> 1
  | `V2  -> 2
  | `V3  -> 3
  | `V11 -> 11
  | `V12 -> 12
  | `V13 -> 13
;;

let ver_to_uint8        (ver:version) : uint8 =
  Uint8.of_int (ver_to_int ver)
;;

let ver_to_uint16       (ver:version) : uint16 =
  Uint16.of_int (ver_to_int ver)
;;

let ver_to_string       (ver:version) : string =
  Conv_utils.uint8_to_string (ver_to_uint8 ver)
;;

let ver_to_human_string (ver:version) : string =
  string_of_int (ver_to_int ver)
;;

let ver_to_block_size   (ver:version) : int =
  match ver with
  | `V1  -> Param_for_v1.block_size
  | `V2  -> Param_for_v2.block_size
  | `V3  -> Param_for_v3.block_size
  | `V11 -> Param_for_v11.block_size
  | `V12 -> Param_for_v12.block_size
  | `V13 -> Param_for_v13.block_size
;;

let ver_to_data_size    (ver:version) : int =
  match ver with
  | `V1  -> Param_for_v1.data_size
  | `V2  -> Param_for_v2.data_size
  | `V3  -> Param_for_v3.data_size
  | `V11 -> Param_for_v11.data_size
  | `V12 -> Param_for_v12.data_size
  | `V13 -> Param_for_v13.data_size
;;

let ver_to_max_file_size (ver:version) : int64 =
  let open Int64 in
  mul (of_int (ver_to_data_size ver)) Common_param.max_blocks_num
;;

let string_to_ver       (str:string)  : (version, string) result =
  match str with
  | "1"  -> Ok `V1
  | "2"  -> Ok `V2
  | "3"  -> Ok `V3
  | "11" -> Ok `V11
  | "12" -> Ok `V12
  | "13" -> Ok `V13
  | _    -> Error "Invalid version string"
;;

let is_reedsolomon_enabled (ver:version) : bool =
  match ver with
  | `V11 | `V12 | `V13 -> true
  | _ -> false
;;
