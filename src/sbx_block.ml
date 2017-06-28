open Stdint
open Crcccitt
open Sbx_version
open Exception

module Header : sig
  type common_header
  type header

  val to_bytes_big_endian : header -> bytes
end = struct
  type common_header =
    { signature  : bytes
    ; version    : version
    ; file_uid   : bytes
    }

  type header =
    { com_head   : common_header
    ; crc16ccitt : uint16 option
    ; seq_num    : uint32 option
    }
end;;

module Encode

type encoded_block =
  { header    : header
  ; data      : bytes
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

type t = Meta of metadata_block | Data of data_block

type res = (t, string) result

let gen_file_uid ~(ver:version) : bytes =
  let uid_len   = ver_to_file_uid_len ver in
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

let make_comm_header ~(ver:version) ?(uid:bytes option) : common_header result =
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

let make_metadata_header ~(ver:version) ~(com_head:common_header) : header =
  { com_head   = com_head
  ; crc16ccitt = None
  ; seq_num    = None
  }
;;

let make_metadata_block ~(ver:version) ~(com_head:common_header) ~(fields:metadata list) : block =
  let data = encode_metadata_list in
  { header = make_metadata_header ~ver ~com_head

let verify (block : t) (block = 
