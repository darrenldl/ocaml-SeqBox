open Sbx_version
open Sbx_block
open Stream_file

type stats = { blocks_decoded : int
             }

module Processor : sig
  (* Resulting decoder may do Printf.printf to report progress, errors etc *)
  val decoder : Core.In_channel.t -> Core.Out_channel.t -> stats
end

module Process : sig
  val decode_file : in_filename:string -> out_filename:string -> (stats, string) result
end
