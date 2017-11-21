(* Data flow diagram

   Disk I/O -> File reader -> Duplicator -->    Encoder   -> File Writer -> Disk I/O
                                         \-> Hasher >-/
 *)
open Stdint
open Sbx_block
open Sbx_specs
open Multihash
open Int64_ops
open Actors

exception File_metadata_get_failed

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

let pack_data (stats:stats) (common:Header.common_fields) (chunk:string) : string =
  let seq_num = Uint32.of_int64 (stats.data_blocks_written <+> 1L) (* always off by +1 *) in
  let block   = Block.make_data_block ~seq_num common ~data:chunk in
  Block.to_string block
;;

let make_dummy_metadata_block_string
    (common : Header.common_fields)
    (metadata_list : Metadata.t list)
    (hash_type     : Multihash.hash_type option)
  : string =
  let open Metadata in
  let fields_except_hash =
    List.filter (function | HSH _ -> false | _ -> true) metadata_list in
  let dummy_fields =
    match hash_type with
    | None -> fields_except_hash
    | Some h ->
      let dummy_hash_bytes           = Multihash.make_dummy_hash_bytes h in
  (HSH dummy_hash_bytes) :: fields_except_hash in
  let dummy_metadata_block       = Block.make_metadata_block common ~fields:dummy_fields in
  Block.to_string dummy_metadata_block
;;

let make_metadata_block_string
    (common : Header.common_fields)
    (metadata_list : Metadata.t list)
    (hash_bytes    : Multihash.hash_bytes option)
  : string =
  let open Metadata in
  let fields_except_hash =
    List.filter (function | HSH _ -> false | _ -> true) metadata_list in
  let fields =
    match hash_bytes with
    | None -> fields_except_hash
    | Some h -> (HSH h) :: fields_except_hash in
  let metadata_block =
    Block.make_metadata_block common ~fields in
  Block.to_string metadata_block
;;

let test_hash_type (hash_type : hash_type option) : unit =
  match hash_type with
  | None -> ()
  | Some h -> Hash.init h |> ignore
;;

(* convert chunk to sbx block *)
let gen_encoder
    ~(common : Header.common_fields)
    ~(hash_type:hash_type option)
    ~(metadata:(Metadata.t list) option)
    ~(in_queue  : string option Lwt_queue.t)
    ~(hash_queue : Multihash.hash_bytes option Lwt_queue.t)
    ~(out_queue : Writer.write_req option Lwt_queue.t)
  : (unit -> (unit, string) result Lwt.t) =
  (fun () ->
     let put_dummy_metadata_string () : unit Lwt.t =
       match metadata with
       | None -> Lwt.return_unit
       | Some lst ->
         let str =
           make_dummy_metadata_block_string common lst hash_type in
         Lwt_queue.put out_queue (Some (No_location str)) in
     let put_metadata_string () : unit Lwt.t =
       match metadata with
       | None -> Lwt.return_unit
       | Some lst ->
         let%lwt hash_bytes =
           Lwt_queue.take hash_queue in
         let str =
           make_metadata_block_string common lst hash_bytes in
         Lwt_queue.put out_queue (Some (With_location (0L, str))) in
     let ver = Header.common_fields_to_ver common in
     let stats = Stats.make_blank_stats ~ver in
     let rec data_loop () : unit Lwt.t =
       match%lwt Lwt_queue.take in_queue with
       | None -> Lwt_queue.put out_queue None
       | Some raw_data ->
         let block_bytes = pack_data stats common raw_data in
         Lwt_queue.put out_queue (Some (No_location block_bytes)) >>
         data_loop () in
     try
       test_hash_type hash_type;
       put_dummy_metadata_string () >>
       data_loop () >>
       put_metadata_string () >>
       Lwt.return_ok ()
     with
     | Sbx_block.Header.Invalid_uid_length ->
       Lwt.return_error "Invalid uid length"
     | Metadata.Too_much_data msg -> Lwt.return_error msg
     | Hash.Unsupported_hash ->
       Lwt.return_error "Hash type is not supported"
  )
;;

let gen_hasher
    ~(in_queue  : string option Lwt_queue.t)
    ~(hash_type : Multihash.hash_type option)
    ~(out_queue : Multihash.hash_bytes option Lwt_queue.t)
  : (unit -> unit Lwt.t) =
  (fun () ->
     try
       match hash_type with
       | None -> Lwt_queue.put out_queue None
       | Some hash_type ->
         let ctx = Hash.init hash_type in
         let rec data_loop () : unit Lwt.t =
           match%lwt Lwt_queue.take in_queue with
           | None ->
             Lwt_queue.put out_queue (Some (Hash.get_hash_bytes ctx))
           | Some data ->
             Hash.feed ctx data;
             data_loop () in
         data_loop ()
     with
     | Hash.Unsupported_hash ->
       (* error reporting for unsupported hash is done by encoder *)
       Lwt.return_unit 
  )
;;

module Process = struct
  let encode_file ~(uid:string option) ~(want_meta:bool) ~(ver:version) ~(hash:string) ~(in_filename:string) ~(out_filename:string) : (stats, string) result =
    Ok (Stats.make_blank_stats ~ver)
  ;;
end
