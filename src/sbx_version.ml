(* Only version 1 is supported as of time of writing *)
type version = V1

module Param_for_v1 = struct
  let file_uid_len = 10
  let signature    = "SBx"
  let block_size   = 512
end

let ver_to_int (ver:version)    : int = function
  | V1 -> 1
;;

let ver_to_uint16 (ver:version) : uint16 =
  Uint16.of_int (ver_to_int ~ver)
;;

let ver_to_file_uid_len (ver:version) : int = function
  | V1 -> Param_for_v1.file_uid_len
;;

let ver_to_signature (ver:version) : bytes = function
  | V1 -> Param_for_v1.signature
;;


