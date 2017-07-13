val seconds_to_hms   : int -> int * int * int

val print_generic : header:string -> (start_time:float -> units_so_far:int64 -> total_units:int64 -> percent:int -> unit)
