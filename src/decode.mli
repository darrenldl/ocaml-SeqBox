open Sbx_specs
open Sbx_block
open Stream_file

type stats = { block_size            : int
             ; blocks_processed      : int64
             ; meta_blocks_decoded   : int64
             ; data_blocks_decoded   : int64
             ; blocks_failed         : int64
             ; failed_block_pos_list : int64 list
             }

val print_stats : stats -> unit

module Processor : sig
  (* Resulting decoder may do Printf.printf to report progress, errors etc *)
  val decoder : Core.In_channel.t -> Core.Out_channel.t -> stats * (int64 option)
end

module Process : sig
  val decode_file : in_filename:string -> out_filename:string option -> (stats, string) result
end
