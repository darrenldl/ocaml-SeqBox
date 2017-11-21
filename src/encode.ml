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
open Actor_utils

exception File_metadata_get_failed
exception Packaged_exn of string

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

module Progress = struct
  let { print_progress = report_encode; _ } : (unit, stats, stats * string) Progress_report.progress_print_functions =
    Progress_report.gen_print_generic
      ~header:"Data encoding progress"
      ~silence_settings:Dynamic_param.Common.silence_settings
      ~display_while_active:Progress_report_param.Encode.Encode_progress.display_while_active
      ~display_on_finish:Progress_report_param.Encode.Encode_progress.display_on_finish
      ~display_on_finish_early:Progress_report_param.Encode.Encode_progress.display_on_finish_early
      ~unit:"chunks"
      ~print_interval:Progress_report_param.Encode.progress_report_interval
      ~eval_start_time:Sys.time
      ~eval_units_so_far:(fun stats -> stats.Stats.blocks_written)
      ~eval_total_units:
        (fun (stats, filename) ->
          let data_size       = Int64.of_int stats.Stats.data_size in
          let total_file_size = File_utils.getsize filename in
          Int64.div
            (Int64.add total_file_size (Int64.sub data_size 1L))
            data_size
        )
  ;;
end

let pack_data (stats:stats) (common:Header.common_fields) (chunk:string) : string =
  let seq_num = Uint32.of_int64 (stats.data_blocks_written <+> 1L) (* always off by +1 *) in
  let block   = Block.make_data_block ~seq_num common ~data:chunk in
  Block.to_string block
;;

let make_dummy_metadata_block_string
    (common : Header.common_fields)
    (metadata_list : Metadata.t list)
    (hash_type     : Multihash.hash_type)
  : string =
  let open Metadata in
  let fields_except_hash =
    List.filter (function | HSH _ -> false | _ -> true) metadata_list in
  let dummy_fields =
    let dummy_hash_bytes           = Multihash.make_dummy_hash_bytes hash_type in
    (HSH dummy_hash_bytes) :: fields_except_hash in
  let dummy_metadata_block       = Block.make_metadata_block common ~fields:dummy_fields in
  Block.to_string dummy_metadata_block
;;

let make_metadata_block_string
    (common : Header.common_fields)
    (metadata_list : Metadata.t list)
    (hash_bytes    : Multihash.hash_bytes)
  : string =
  let open Metadata in
  let fields_except_hash =
    List.filter (function | HSH _ -> false | _ -> true) metadata_list in
  let fields =
    (HSH hash_bytes) :: fields_except_hash in
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
    ~(hash_type:hash_type)
    ~(metadata:(Metadata.t list) option)
    ~(in_queue  : string option Lwt_queue.t)
    ~(hash_queue : Multihash.hash_bytes Lwt_queue.t)
    ~(out_queue : Writer.write_req option Lwt_queue.t)
    ~(filename : string)
  : (unit -> (stats, string) result Lwt.t) =
  (fun () ->
     let ver = Header.common_fields_to_ver common in
     let stats = Stats.make_blank_stats ~ver in
     let put_dummy_metadata_string () : unit Lwt.t =
       match metadata with
       | None -> Lwt.return_unit
       | Some lst ->
         let str =
           make_dummy_metadata_block_string common lst hash_type in
         Lwt_queue.put out_queue (Some (No_position str)) in
     let put_metadata_string () : unit Lwt.t =
       match metadata with
       | None -> Lwt.return_unit
       | Some lst ->
         let%lwt hash_bytes =
           Lwt_queue.take hash_queue in
         let str =
           make_metadata_block_string common lst hash_bytes in
         Stats.add_written_meta_block stats;
         Lwt_queue.put out_queue (Some (With_position (0L, str))) in
     let rec data_loop () : unit Lwt.t =
       Progress.report_encode
         ~start_time_src:() ~units_so_far_src:stats ~total_units_src:(stats, filename);
       match%lwt Lwt_queue.take in_queue with
       | None -> Lwt.return_unit
       | Some raw_data ->
         let block_bytes = pack_data stats common raw_data in
         Stats.add_written_data_block stats ~data_len:(String.length raw_data);
         Lwt_queue.put out_queue (Some (No_position block_bytes)) >>
         data_loop () in
     try
       put_dummy_metadata_string () >>
       data_loop () >>
       put_metadata_string () >>
       Lwt_queue.put out_queue None >>
       Lwt.return_ok stats
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
    ~(hash_type : Multihash.hash_type)
    ~(out_queue : Multihash.hash_bytes Lwt_queue.t)
  : (unit -> unit Lwt.t) =
  (fun () ->
     try
       let ctx = Hash.init hash_type in
       let rec data_loop () : unit Lwt.t =
         match%lwt Lwt_queue.take in_queue with
         | None ->
           Lwt_queue.put out_queue (Hash.get_hash_bytes ctx)
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
  let get_file_metadata ~(in_filename:string) ~(out_filename:string) : Metadata.t list =
    try
      let open Metadata in
      let open File_utils in
      let open Time_utils in
      [ FNM (Misc_utils.path_to_file in_filename)
      ; SNM (Misc_utils.path_to_file out_filename)
      ; FSZ (getsize_uint64  ~filename:in_filename)
      ; FDT (getmtime_uint64 ~filename:in_filename)
      ; SDT (gettime_uint64 ())
      ]
    with
    | _ -> raise File_metadata_get_failed
  ;;

  let encode_file ~(uid:string option) ~(want_meta:bool) ~(ver:version) ~(hash:string) ~(in_filename:string) ~(out_filename:string) : (stats, string) result =
    try
      let actor_network_setup : (stats, string) result Lwt.t =
        (* params setup *)
        let common =
          match uid with
          | Some uid -> Header.make_common_fields ~uid ver
          | None     -> Header.make_common_fields      ver in
        let metadata =
          if want_meta then
            Some (get_file_metadata ~in_filename ~out_filename)
          else
            None in
        let chunk_size = ver_to_data_size ver in
        let hash_type =
          match string_to_hash_type hash with
          | Error msg -> raise (Packaged_exn msg)
          | Ok h -> test_hash_type (Some h); h in

        (* communication queues setup *)
        let read_to_dup_q  = Lwt_queue.create ~init_val:None 100 in
        let dup_to_enc_q   = Lwt_queue.create ~init_val:None 100 in
        let dup_to_hash_q  = Lwt_queue.create ~init_val:None 100 in
        let hash_to_enc_q  =
          Lwt_queue.create ~init_val:(make_dummy_hash_bytes hash_type) 100 in
        let enc_to_write_q = Lwt_queue.create ~init_val:None 100 in
        let write_reply_q  =
          Lwt_queue.create ~init_val:(Writer.Position 0L) 100 in

        (* result monitor queues setup *)
        let result_q : (stats option, string) result Lwt_queue.t =
          Lwt_queue.create ~init_val:(Ok None) 100 in

        (* actors setup *)
        let waiter, wakener = Lwt.wait () in

        let reader = Lwt.bind waiter
            (gen_file_reader
               ~filename:in_filename
               ~chunk_size
               ~out_queue:read_to_dup_q) in

        (*let duplicator = Lwt.bind waiter*)
        Lwt.async (gen_duplicator
                     ~in_queue:read_to_dup_q
                     ~out_queues:[dup_to_hash_q; dup_to_enc_q]
                     ~stop_pred:(fun x -> x = None)
                     ~forward_stopper:true);

        let encoder = Lwt.bind waiter
            (gen_encoder
               ~common
               ~hash_type
               ~metadata
               ~in_queue:dup_to_enc_q
               ~hash_queue:hash_to_enc_q
               ~out_queue:enc_to_write_q
               ~filename:in_filename) in

        let hasher = Lwt.bind waiter
            (gen_hasher
               ~in_queue:dup_to_hash_q
               ~hash_type
               ~out_queue:hash_to_enc_q) in

        let writer = Lwt.bind waiter
            (gen_file_writer
               ~filename:out_filename
               ~in_queue:enc_to_write_q
               ~reply_queue:write_reply_q) in

        (* bind results of actors to error monitor queue *)
        let reader_mon = bind_to_queue
            ~convert:(function
                | Ok () -> Ok None
                | Error msg -> Error msg)
          reader result_q in

        let encoder_mon = bind_to_queue
            ~convert:(function
                | Ok x -> Ok (Some x)
                | Error msg -> Error msg)
            encoder result_q in

        let hasher_mon = bind_to_queue
            ~convert:(fun _ -> Ok None)
            hasher result_q in

        let writer_mon = bind_to_queue
            ~convert:(function
                | Ok () -> Ok None
                | Error msg -> Error msg)
            writer result_q in

        let mon_list = [
          reader_mon;
          encoder_mon;
          hasher_mon;
          writer_mon
        ] in

        (* start actors *)
        Lwt.wakeup wakener ();

        let%lwt results =
          monitor
            ~count:(List.length mon_list)
            ~fail_pred:(function
                | Error _ -> true
                | Ok _    -> false)
            ~monitor_queue:result_q in

        let errors = List.filter (function | Error _ -> true | _ -> false) results in
        let okays  = List.filter (function | Ok (Some _) -> true | _ -> false) results in

        if List.length errors > 0 then
          match List.hd errors with
          | Error x -> Lwt.return_error x
          | _       -> assert false
        else
          match List.hd okays with
          | Ok (Some x) -> Lwt.return_ok x
          | _           -> assert false

      (*Lwt.return (Ok (Stats.make_blank_stats ~ver)) *)
      in
      Lwt_main.run actor_network_setup
    with
    | File_metadata_get_failed            -> Error "Failed to get file metadata"
    | Sbx_block.Header.Invalid_uid_length -> Error "Invalid uid length"
    | Packaged_exn msg                    -> Error msg
  ;;
end
