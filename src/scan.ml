open Sbx_block
open Std_int

module Stats = struct
  type t = { bytes_processed       : int64
           ; blocks_processed      : int64
           ; meta_blocks_processed : int64
           ; data_blocks_processed : int64
           ; start_time            : float
           }

  let make_blank_stats () : t =
    { bytes_processed       = 0L
    ; blocks_processed      = 0L
    ; meta_blocks_processed = 0L
    ; data_blocks_processed = 0L
    ; start_time            = Sys.time ()
    }
  ;;

  let add_bytes (stats:t) ~(num:int64) : t =
    { bytes_processed       = stats.bytes_processed       <+> num
    ; blocks_processed      = stats.blocks_processed
    ; meta_blocks_processed = stats.meta_blocks_processed
    ; data_blocks_processed = stats.data_blocks_processed
    ; start_time            = stats.start_time
    }
  ;;

  let add_meta_block (stats:t) : t =
    { bytes_processed       = stats.bytes_processed
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_processed = stats.meta_blocks_processed <+> 1L
    ; data_blocks_processed = stats.data_blocks_processed
    ; start_time            = stats.start_time
    }
  ;;

  let add_data_block (stats:t) : t =
    { bytes_processed       = stats.bytes_processed
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_processed = stats.meta_blocks_processed
    ; data_blocks_processed = stats.data_blocks_processed <+> 1L
    ; start_time            = stats.start_time
    }
  ;;
end

type stats = Stats.t

module Progress = struct
  let { print_progress = report_scan; _ } : (unit, stats, in_channel) Progress_report.progress_print_functions =
    Progress_report.gen_print_generic
      ~header:"Scan progress"
      ~silence_settings:Param.Common.silence_settings
      ~display_while_active:Param.Scan.Scan_progress.display_while_active
      ~display_on_finish:Param.Scan.Scan_progress.display_on_finish
      ~display_on_finish_early:Param.Scan.Scan_progress.display_on_finish_early
      ~unit:"bytes"
      ~print_interval:Param.Rescue.progress_report_interval
      ~eval_start_time:Sys.time
      ~eval_units_so_far:(fun stats -> stats.Stats.bytes_processed)
      ~eval_total_units:(fun in_file -> LargeFile.in_channel_length in_file)
end

module Processor = struct
  let scan_proc ~(only_pick:Block.block_type) ~(stats:stats) (in_file:in_channel) : stats * (Block.t option) =
    let open Read_chunk in
    let raw_header_pred = Sbx_block_helpers.block_type_to_raw_header_pred only_pick in
    let rec scan_proc_internal (stats:stats) (result_so_far:Block.t option) : stats * (Block.t option) =
      (* report progress *)
      Progress.report_scan ~start_time_src:() ~unit_so_far_src:stats ~total_units_src:in_file;
      match result_so_far with
      | Some _ as x -> (stats, x)
      | None        ->
        let (read_len, block) = Processor_components.try_get_block_from_in_channel ~raw_header_pred in_file in
        if read_len = 0L then
          (stats, result_so_far)
        else
          let new_stats = Stats.add_bytes stats ~num:read_len in
          scan_proc_internal new_stats block in
    scan_proc_internal stats None
  ;;

  let write_proc ~(stats:stats) ~(block:block) (out_file:out_channel) : unit =
    let open Write_chunk in
    let byte : string = Int64.to_string stats.bytes_processed in
    let ver  : string = ver_to_string (block_to_ver block) in
    let uid  : string = Conv_utils.bytes_to_hex_string_uid (block_to_file_uid block) in
    let seq  : string =
      match block_to_seq_num block with
      | None   -> assert false
      | Some n -> Uint32.to_string n in
    let chunk = String.concat "" [(String.concat ", " [ byte
                                                      ; ver
                                                      ; uid
                                                      ; seq
                                                      ])
                                 ; "\n"] in
    write out_file ~chunk
  ;;
end

module Process = struct
end
