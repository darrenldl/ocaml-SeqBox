type t

type common_header = Header.common_header

exception Length_mismatch of string;

(* Only version 1 is supported as of time of writing *)
type version = [ `One ]

type block_type = [ `Meta | `Data | `Last_data ]

type res = (t, string) result

