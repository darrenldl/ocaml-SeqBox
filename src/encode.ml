(* Data flow diagram

   Disk I/O -> File reader -> Duplicator -->    Encoder   -> File Writer -> Disk I/O
                                         \-> Hasher >-/
 *)
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

type stats = Stats.t

(* convert chunk to sbx block *)
let gen_encoder
    ~(common : Header.common_fields)
    ~(in_queue  : string option Lwt_queue.t)
    ~(out_queue : string option Lwt_queue.t)
  : (unit -> unit Lwt.t) =
  let pack_data (stats:stats) (common:Header.common_fields) (chunk:string) : string =
    let seq_num = Uint32.of_int64 (stats.data_blocks_written <+> 1L) (* always off by +1 *) in
    let block   = Block.make_data_block ~seq_num common ~data:chunk in
    Block.to_string block in
  (fun () ->
     let rec loop () : unit Lwt.t =
       let ver = Header.common_fields_to_ver common in
       let stats = Stats.make_blank_stats ~ver in
       match%lwt Lwt_queue.take in_queue with
       | None -> Lwt_queue.put out_queue None
       | Some raw_data ->
         let block_bytes = pack_data stats common raw_data in
         Lwt_queue.put out_queue (Some block_bytes) >>
         loop () in
     loop ()
  )
;;

let gen_hasher
    ~()
