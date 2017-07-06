open Stdint
open Misc_utils

(* Only version 1 is supported as of time of writing *)
type version = [ `V1 ]

module Common_param = struct
  let file_uid_len : int   = 6
  let signature    : bytes = "SBx"
  let header_size  : int   = 16
end

module Param_for_v1 = struct
  let block_size   : int   = 512
  let data_size    : int   = block_size - Common_param.header_size
end

let sbx_file_uid_len = Common_param.file_uid_len;;

let sbx_signature    = Common_param.signature;;

let sbx_header_size  = Common_param.header_size;;

let ver_to_int          (ver:version) : int =
  match ver with
  | `V1 -> 1
;;

let ver_to_uint8        (ver:version) : uint8 =
  Uint8.of_int (ver_to_int ver)
;;

let ver_to_uint16       (ver:version) : uint16 =
  Uint16.of_int (ver_to_int ver)
;;

let ver_to_bytes        (ver:version) : bytes =
  let buf = Bytes.make 2 '\x00' in
  Uint16.to_bytes_big_endian (ver_to_uint16 ver) buf 0;
  buf
;;

let ver_to_block_size   (ver:version) : int =
  match ver with
  | `V1 -> Param_for_v1.block_size
;;

let ver_to_data_size    (ver:version) : int =
  match ver with
  | `V1 -> Param_for_v1.data_size
;;

