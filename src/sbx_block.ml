open Stdint

(* Only version 1 is supported as of time of writing *)
type version = V1

module Param_for_v1 : sig
  val file_uid_len : int
  val signature    : bytes
end = struct
  let file_uid_len = 10
  let signature    = "SBx"
end

exception Length_mismatch of string;

type common_header =
  { signature : bytes
  ; version   : version
  ; file_uid  : bytes
  }

type header =
  { com_head   : common_header
  ; crc16ccitt : uint16
  ; seq_num    : uint32
  }

type metadata_id = FNM | SNM | FSZ | FDT | SDT | HSH | PID

type metadata =
  { id   : meta_id
  ; len  : uint8
  ; data : bytes
  }

type metadata_block =
  { header : header
  ; data   : metadata list
  }

type data_block =
  { header : header
  ; data   : bytes
  }

type last_data_block =
  { header  : header
  ; data    : bytes
  ; padding : bytes
  }

type block_type = [ `Meta | `Data | `Last_data ]

type t = metadata_block | data_block | last_data_block

type res = (t, string) result

let gen_file_uid ~(ver:version) : bytes =
  let uid_len   =
    match ver with
    | V1 -> Param_for_v1.file_uid_len in
  let gen_bytes () : bytes =
    Cstruct.to_string (
      Nocrypto.Rng.generate ~g:!Nocrypto.Rng.generator uid_len
    ) in
  try
    gen_bytes ()
  with
  | Uncommon.Boot.Unseeded_generator ->
    begin
      let () = Nocrypto_entropy_unix.initialize () |> ignore;;
      gen_bytes ()
    end
;;

let create_comm_header ~(ver:version) ?(uid:bytes option) : common_header result =
  let uid = match uid with
    | Some x ->
      let len =
        match ver with
        | V1 -> Param_for_v1.file_uid_len in
      if Bytes.length x == len then
        x
      else
        raise (Length_mismatch "length of provided UID does not match specification")
    | None   -> gen_file_uid ~ver in
  { signature = Param_for_v1.signature
  ; version   = ver
  ; file_uid  = uid }
;;

let create_metadata_header ~(com_head:common_header) ~(fields:metadata list) : header =
  { com_head = com_head
  ; 

let verify (block : t) (block = 
