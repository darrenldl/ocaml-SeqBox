open Sbx_block
open Stream_file

module Stats : sig
  type t = { bytes_processed   : int64
           ; meta_blocks_found : int64
           ; start_time        : float
           }
end

type stats = Stats.t

module Process : sig
  val fetch_single_meta : in_filename:string -> ((Block.t * int64) option, string) result

  val fetch_multi_meta  : in_filename:string -> ((Block.t * int64) list,   string) result
end
