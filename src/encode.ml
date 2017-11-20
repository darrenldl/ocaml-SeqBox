open Stdint
open Sbx_block
open Sbx_specs
open Multihash
open Int64_ops

module Stats = struct
  type t = {         block_size          : int
           ;         data_size           : int
           ; mutable blocks_written      : int64
           ; mutable meta_blocks_written : int64
           ; mutable data_blocks_written : int64
           ; mutable total_data_encoded  : int64
           ;         start_time          : float 
           }

  let make_blank_stats ~(ver:version) : t =
    { block_size          = ver_to_block_size ver
    ; data_size           = ver_to_data_size  ver
    ; blocks_written      = 0L
    ; meta_blocks_written = 0L
    ; data_blocks_written = 0L
    ; total_data_encoded  = 0L
    ; start_time          = Sys.time ()
    }

  let add_written_meta_block (stats:t) : unit =
    stats.blocks_written      <- stats.blocks_written <+> 1L;
    stats.meta_blocks_written <- stats.meta_blocks_written <+> 1L
  ;;

  let add_written_data_block (stats:t) ~(data_len:int) : unit =
    stats.blocks_written      <- stats.blocks_written      <+> 1L;
    stats.data_blocks_written <- stats.data_blocks_written <+> 1L;
    stats.total_data_encoded  <- stats.total_data_encoded  <+> (Int64.of_int data_len)
  ;;

  let print_stats (stats:t) : unit =
    Printf.printf "Block size used in encoding         : %d\n"  stats.block_size;
    Printf.printf "Data  size used in encoding         : %d\n"  stats.data_size;
    Printf.printf "Number of blocks written            : %Ld\n" stats.blocks_written;
    Printf.printf "Number of blocks written (metadata) : %Ld\n" stats.meta_blocks_written;
    Printf.printf "Number of blocks written (data)     : %Ld\n" stats.data_blocks_written;
    Printf.printf "Amount of data encoded (in bytes)   : %Ld\n" stats.total_data_encoded;
    let (hour, minute, second) = Progress_report.Helper.seconds_to_hms (int_of_float (Sys.time() -. stats.start_time)) in
    Printf.printf "Time elapsed                        : %02d:%02d:%02d\n" hour minute second
  ;;
end

(* convert chunk to sbx block *)
let gen_encoder
    ~(in_queue  : string Lwt_queue.t)
    ~(out_queue : Sbx_block.Block.t Lwt_queue.t)
  : (unit -> unit Lwt.t) =
  (fun () ->
     let%lwt raw_data = Lwt_queue.take in_queue in
      
  )
;;

