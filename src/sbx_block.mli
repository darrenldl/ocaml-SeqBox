type header        = Header.t

type header_common = Header.common

type block         = Block.t

module Header : sig
  type t
  type common_fields
  val to_bytes_big_endian : header -> bytes
end

module Block : sig
  type t
  (* chunk_to_block : Header.common -> *)
end

type common_header = Header.common_header

exception Length_mismatch of string;

(* Only version 1 is supported as of time of writing *)
type version = [ `One ]

type block_type = [ `Meta | `Data | `Last_data ]

type res = (t, string) result

