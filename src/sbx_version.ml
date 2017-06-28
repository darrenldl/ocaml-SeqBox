open Stdint

(* Only version 1 is supported as of time of writing *)
type version = [ `V1 ]

module Param_for_v1 = struct
  let file_uid_len = 10
  let signature    = Bytes.of_string "SBx"
  let block_size   = 512
  let header_size  = 16
  let data_size    = block_size - header_size
end

let ver_to_int (ver:version)    : int =
  match ver with
  | `V1 -> 1
;;

let ver_to_uint16       (ver:version) : uint16 =
  Uint16.of_int (ver_to_int ver)
;;

let ver_to_file_uid_len (ver:version) : int =
  match ver with
  | `V1 -> Param_for_v1.file_uid_len
;;

let ver_to_signature    (ver:version) : bytes =
  match ver with
  | `V1 -> Param_for_v1.signature
;;

let ver_to_block_size   (ver:version) : int =
  match ver with
  | `V1 -> Param_for_v1.block_size
;;

let ver_to_header_size  (ver:version) : int =
  match ver with
  | `V1 -> Param_for_v1.header_size
;;

let ver_to_data_size    (ver:version) : int =
  match ver with
  | `V1 -> Param_for_v1.data_size
;;


