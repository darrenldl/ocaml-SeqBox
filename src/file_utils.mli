open Stdint

val getmtime        : filename:string -> (float,  string) result

val getmtime_uint64 : filename:string -> (uint64, string) result
