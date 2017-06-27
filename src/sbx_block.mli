type t

(* Only version 1 is supported as of time of writing *)
type version = [ `One ]

type block_type = [ `Meta | `Data | `Last_data ]

type res = (t, string) result

val create : ver:int -> block_type:block_type -> res
