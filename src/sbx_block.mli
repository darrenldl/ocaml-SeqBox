type t

type version = [ `One ]

type block_type = [ `Meta | `Data | `Last_data ]

val create : ver:int -> block_type:block_type -> result
