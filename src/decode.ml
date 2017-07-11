open Stdint
open Nocrypto.Hash
open Sbx_specs
open Sbx_block
open Stream_file

let (<+>) = Int64.add;;

let (<->) = Int64.sub;;

let (<*>) = Int64.mul;;

module Stats = struct
  type t = { block_size            : int
           ; blocks_processed      : int64
           ; meta_blocks_decoded   : int64
           ; data_blocks_decoded   : int64
           ; blocks_failed         : int64
           ; failed_block_pos_list : int64 list
           ; recorded_hash         : bytes option
           ; output_file_hash      : bytes option
           }

  let make_blank_stats ~(ver:version) : t =
    { block_size            = ver_to_block_size ver
    ; blocks_processed      = 0L
    ; meta_blocks_decoded   = 0L
    ; data_blocks_decoded   = 0L
    ; blocks_failed         = 0L
    ; failed_block_pos_list = []
    ; recorded_hash         = None
    ; output_file_hash      = None
    }
  ;;

  let add_okay_meta_block (stats:t) : t =
    { block_size            = stats.block_size
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_decoded   = stats.meta_blocks_decoded   <+> 1L
    ; data_blocks_decoded   = stats.data_blocks_decoded
    ; blocks_failed         = stats.blocks_failed
    ; failed_block_pos_list = stats.failed_block_pos_list
    ; recorded_hash         = stats.recorded_hash
    ; output_file_hash      = stats.output_file_hash
    }
  ;;

  let add_okay_data_block (stats:t) : t =
    { block_size            = stats.block_size
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_decoded   = stats.meta_blocks_decoded
    ; data_blocks_decoded   = stats.data_blocks_decoded   <+> 1L
    ; blocks_failed         = stats.blocks_failed
    ; failed_block_pos_list = stats.failed_block_pos_list
    ; recorded_hash         = stats.recorded_hash
    ; output_file_hash      = stats.output_file_hash
    }
  ;;

  let add_failed_block (stats:t) : t =
    { block_size            = stats.block_size
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_decoded   = stats.meta_blocks_decoded
    ; data_blocks_decoded   = stats.data_blocks_decoded
    ; blocks_failed         = stats.blocks_failed         <+> 1L
    ; failed_block_pos_list =
        if stats.blocks_failed < 500L then
          stats.blocks_processed :: stats.failed_block_pos_list
        else
          stats.failed_block_pos_list
    ; recorded_hash         = stats.recorded_hash
    ; output_file_hash      = stats.output_file_hash
    }
  ;;
  
  let add_hashes ~(recorded_hash:bytes option) ~(output_file_hash:bytes option) (stats:t) : t =
    { block_size            = stats.block_size
    ; blocks_processed      = stats.blocks_processed
    ; meta_blocks_decoded   = stats.meta_blocks_decoded
    ; data_blocks_decoded   = stats.data_blocks_decoded
    ; blocks_failed         = stats.blocks_failed
    ; failed_block_pos_list = stats.failed_block_pos_list
    ; recorded_hash         =
        begin
          match (stats.recorded_hash, recorded_hash) with
          | (Some hsh, Some _)   -> Some hsh
          | (None,     Some hsh) -> Some hsh
          | (Some hsh, None)     -> Some hsh
          | (None,     None)     -> None
        end
    ; output_file_hash      =
        begin
          match (stats.output_file_hash, output_file_hash) with
          | (Some hsh, Some _)   -> Some hsh
          | (None,     Some hsh) -> Some hsh
          | (Some hsh, None)     -> Some hsh
          | (None,     None)     -> None
        end
    }
  ;;

  let print_failed_pos (block_size:int) (pos_list:int64 list) : unit =
    let block_size = Int64.of_int block_size in
    List.iter (fun x -> Printf.printf "Failed to decode block %Ld, at %Ld bytes\n" x (block_size <*> x)) (List.rev pos_list)
  ;;

  let print_stats (stats:t) : unit =
    Printf.printf "Block size used in decoding                    : %d\n"  stats.block_size;
    Printf.printf "Number of          blocks processed            : %Ld\n" stats.blocks_processed;
    Printf.printf "Number of metadata blocks successfully decoded : %Ld\n" stats.meta_blocks_decoded;
    Printf.printf "Number of data     blocks successfully decoded : %Ld\n" stats.data_blocks_decoded;
    Printf.printf "Number of          blocks failed to decode     : %Ld\n" stats.blocks_failed;
    Printf.printf "Recorded hash                                  : %s\n"
      (match stats.recorded_hash    with | Some hsh -> Conv_utils.bytes_to_hex_string hsh | None -> "N/A");
    Printf.printf "Hash of the output file                        : %s\n"
      (match stats.output_file_hash with | Some hsh -> Conv_utils.bytes_to_hex_string hsh | None -> "N/A");
    if stats.meta_blocks_decoded = 0L then
      begin
        print_newline ();
        Printf.printf "Warning : no metadata block was found in the sbx container\n";
        Printf.printf "          it is likely that the output file is not of the correct size\n";
        Printf.printf "          and has data padding bytes attached at the end of it\n";
        print_newline ();
      end;
    begin
      match (stats.recorded_hash, stats.output_file_hash) with
      | (Some recorded_hash, Some output_file_hash) ->
        if (Bytes.compare recorded_hash output_file_hash) = 0 then
          Printf.printf "The output file hash matches the recorded hash\n"
        else
          Printf.printf "The output file hash does NOT match the recorded hash\n"
      | (Some _,             None)                  ->
        Printf.printf "No hash is available for output file\n"
      | (None,               Some _)                ->
        Printf.printf "No recorded hash is available\n"
      | (None,               None)                  ->
        Printf.printf "Neither recorded hash nor output file hash is available\n";
    end;
    Printf.printf "First up to 500 failing positions (block and bytes index start at 0)\n";
    print_failed_pos stats.block_size stats.failed_block_pos_list
  ;;
end

type stats = Stats.t

module Progress = struct
  let report : stats -> Core.In_channel.t -> unit  =
    let print_every_n = Param.Decode.progress_report_interval in
    let report_count  = ref 0 in
    (fun stats in_file ->
       let block_size   : int64 =
         Int64.of_int stats.block_size in
       let total_blocks : int64 =
         Int64.div
           (Int64.add (Core.In_channel.length in_file) (Int64.sub block_size 1L))
           block_size in
       let percent      : int   =
         Int64.to_int (Int64.div
                         (Int64.mul
                            100L
                            stats.blocks_processed)
                         total_blocks) (* the math is okay cause 1 chunk -> 1 block *) in
       if percent = 100 then (* always print if reached 100% *)
         begin
           Printf.printf "\rData decoding progress                         : %Ld / %Ld - %d%%\n" stats.blocks_processed total_blocks percent;
           print_newline ()
       end
       else begin
         if !report_count = 0 then
           Printf.printf "\rData decoding progress                         : %Ld / %Ld - %d%%" stats.blocks_processed total_blocks percent
         else
           () (* do nothing *)
       end;
       (* increase and mod report counter *)
       report_count := !report_count mod print_every_n
    )
  ;;
end

module Processor = struct
  let find_first_block_proc ~(want_meta:bool) (in_file:Core.In_channel.t) : Block.t option =
    let open Read_chunk in
    let len = Param.Decode.ref_block_scan_alignment in 
    let bytes_to_block (raw_header:Header.raw_header) (chunk:bytes) : Block.t option =
      let want_block =
        if want_meta then
          Header.raw_header_is_meta raw_header
        else
          Header.raw_header_is_data raw_header in
      if want_block then
        try
          Some (Block.of_bytes ~raw_header chunk)
        with
        | Header.Invalid_bytes
        | Metadata.Invalid_bytes
        | Block.Invalid_bytes
        | Block.Invalid_size     -> None
      else
        None in
    let rec find_first_block_proc_internal () : Block.t option =
      match read in_file ~len with
      | None           -> None
      | Some { chunk } ->
        if Bytes.length chunk < 16 then
          None  (* no more bytes left in file *)
        else
          let test_header_bytes = Misc_utils.get_bytes chunk ~pos:0 ~len:16 in
          let test_header : Header.raw_header option =
            try
              Some (Header.of_bytes test_header_bytes)
            with
            | Header.Invalid_bytes -> None in
          match test_header with
          | None            -> find_first_block_proc_internal () (* go to next block *)
          | Some raw_header ->
            (* possibly grab more bytes depending on version *)
            let chunk =
              Processor_helpers.patch_block_bytes_if_needed in_file ~raw_header ~chunk in
            let test_block : Block.t option =
              bytes_to_block raw_header chunk in
            match test_block with
            | None       -> find_first_block_proc_internal () (* go to next block *)
            | Some block -> Some block  (* found a valid block *) in
    let res = find_first_block_proc_internal () in
    Core.In_channel.seek in_file 0L;  (* reset seek position *)
    res
  ;;

  (* ref_block will be used as reference for version and uid
   *  block must match those two parameters to be accepted
   *)
  let find_valid_data_block_proc ~(ref_block:Block.t) (in_file:Core.In_channel.t) ~(stats:stats) : stats * (Block.t option) =
    let open Read_chunk in
    let ref_ver      = Block.block_to_ver ref_block in
    let len          = ver_to_block_size ref_ver in
    let ref_file_uid = Block.block_to_file_uid ref_block in
    let rec find_valid_data_block_proc_internal (stats:stats) : stats * Block.t option =
      (* report progress *)
      Progress.report stats in_file;
      match read in_file ~len with
      | None           -> (stats, None)
      | Some { chunk } ->
        let block =
          try
            Some (Block.of_bytes chunk)
          with
          | Header.Invalid_bytes
          | Metadata.Invalid_bytes
          | Block.Invalid_bytes
          | Block.Invalid_size     -> None in
        match block with
        | None       -> find_valid_data_block_proc_internal (Stats.add_failed_block stats) (* move onto finding next block *)
        | Some block ->
          begin
            if Block.is_meta block then (
              (* don't return metadata block *)
              find_valid_data_block_proc_internal (Stats.add_okay_meta_block stats) (* move onto finding next block *) )
            else
              let file_uid = Block.block_to_file_uid block in
              let ver      = Block.block_to_ver      block in
              (* make sure uid and version match *)
              if file_uid = ref_file_uid && ver = ref_ver then
                (Stats.add_okay_data_block stats, Some block)
              else
                find_valid_data_block_proc_internal (Stats.add_failed_block stats) (* move onto finding next block *)
          end in
    find_valid_data_block_proc_internal stats
  ;;

  let output_decoded_data_proc ~(ref_block:Block.t) ~(block:Block.t) (out_file:Core.Out_channel.t) : unit =
    let open Write_chunk in
    if Block.is_meta block then
      ()  (* ignore attempts to write metadata block *)
    else
      (* determine position to write to using reference block and block's sequence number *)
      let ref_ver           = Block.block_to_ver ref_block in
      let data_len  : int64 = Int64.of_int    (ver_to_data_size ref_ver) in
      match Block.block_to_seq_num block with
      | None         -> assert false
      | Some seq_num ->
        let seq_num   : int64 = Uint32.to_int64 seq_num in
        let write_pos : int64 = (seq_num <-> 1L) <*> data_len in  (* seq_num is guaranteed to be > 0 due to above check of is_meta *)
        (* seek to the proper position then write *)
        Core.Out_channel.seek out_file write_pos;
        write out_file ~chunk:(Block.block_to_data block)
  ;;

  let decode_and_output_proc ~(ref_block:Block.t) (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) : stats =
    let rec decode_and_output_proc_internal (stats:stats) : stats =
      match find_valid_data_block_proc ~ref_block in_file ~stats with
      | (stats, None)       -> stats
      | (stats, Some block) ->
        output_decoded_data_proc ~ref_block ~block out_file;
        decode_and_output_proc_internal stats in
    decode_and_output_proc_internal (Stats.make_blank_stats ~ver:(Block.block_to_ver ref_block))
  ;;

  let out_filename_fetcher (in_file:Core.In_channel.t) : string option =
    let metadata_block : Block.t option =
      find_first_block_proc ~want_meta:true in_file in
    match metadata_block with
    | Some block ->
      begin
        let metadata_list  = Metadata.dedup (Block.block_to_meta block) in
        match List.filter (function | Metadata.FNM _ -> true | _ -> false) metadata_list with
        | [ ]       -> None
        | [FNM str] -> Some str
        | [_]       -> assert false
        | _ :: _    -> assert false
      end
    | None       -> None
  ;;

  let decoder (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) : stats * (int64 option) =
    (* find a block to use as reference *)
    let ref_block : Block.t option =
      (* try to find a metadata block first *)
      match find_first_block_proc ~want_meta:true in_file with
      | Some block -> Some block
      | None       -> find_first_block_proc ~want_meta:false in_file (* get the first usable data block *) in
    match ref_block with
    | None           -> raise (Packaged_exn "No usable blocks in file")
    | Some ref_block ->
      (* got a reference block, decode all data blocks *)
      let stats = decode_and_output_proc ~ref_block in_file out_file in
      (* if reference block is a metadata block, then use the recorded file size to indicate truncatation *)
      let truncate : int64 option =
        if Block.is_meta ref_block then
          let metadata_list   = Block.block_to_meta ref_block in
          try
            let open Metadata in
            match List.find (function | FSZ _ -> true | _ -> false) metadata_list with
            | FSZ file_size -> Some (Uint64.to_int64 file_size)
            | _             -> None
          with
          | Not_found -> None
        else
          None in
      let recorded_hash : bytes option =
        let open Metadata in
        let metadata_list      = dedup (Block.block_to_meta ref_block) in
        try
          match List.find (function | HSH _ -> true | _ -> false) metadata_list with
          | HSH (_, raw) -> Some raw
          | _            -> None
        with
        | Not_found -> None in
      let stats = Stats.add_hashes ~recorded_hash ~output_file_hash:None stats in
      (stats, truncate)
  ;;

  let rec hash_proc ?(hash_state:SHA256.t = SHA256.init()) (in_file:Core.In_channel.t) : bytes =
    let open Read_chunk in
    let read_len = 1024 * 1024 (* 1 MiB *) in
    match read in_file ~len:read_len with
    | None           -> Conv_utils.sha256_hash_state_to_bytes hash_state
    | Some { chunk } ->
      SHA256.feed hash_state (Cstruct.of_bytes chunk);
      hash_proc ~hash_state in_file
  ;;

  let hasher (in_file:Core.In_channel.t) : bytes =
    hash_proc in_file 
  ;;
end

module Process = struct
  let fetch_out_filename ~(in_filename:string) ~(out_filename:string option) : (string option, string) result =
    match out_filename with
    | Some str -> Ok (Some str)
    | None     ->
      match Stream.process_in ~in_filename ~processor:Processor.out_filename_fetcher with
      | Ok result -> Ok result
      | Error msg -> Error msg
  ;;

  let hash_file ~(in_filename:string) : (bytes, string) result =
    Stream.process_in ~in_filename ~processor:Processor.hasher
  ;;

  let hash_file_w_warning ~(in_filename:string) : bytes option =
    match hash_file ~in_filename with
    | Ok hash   -> Some hash
    | Error msg -> Printf.printf "Warning : %s\n" msg; None
  ;;

  let decode_file ~(in_filename:string) ~(out_filename:string option) : (stats, string) result =
    match fetch_out_filename ~in_filename ~out_filename with
    | Error msg -> Error msg
    | Ok None   ->
      Error (Printf.sprintf "failed to obtain a filename for output(none is provided and no valid metadata block with filename field is found in %s)" in_filename)
    | Ok (Some out_filename) ->
      match Stream.process_in_out ~append:false ~in_filename ~out_filename ~processor:Processor.decoder with
      | Ok (stats, Some trunc_size) ->
        begin
          try
            Unix.LargeFile.truncate out_filename trunc_size;
            let output_file_hash = hash_file_w_warning ~in_filename:out_filename in
            Ok (Stats.add_hashes ~recorded_hash:None ~output_file_hash stats)
          with
          | _ -> Error "failed to truncate output file"
        end
      | Ok (stats, None)            ->
        let output_file_hash = hash_file_w_warning ~in_filename:out_filename in
        Ok (Stats.add_hashes ~recorded_hash:None ~output_file_hash stats)
      | Error msg                   -> Error msg
  ;;

end

(* let test_decode () =
  let open Metadata in
  match Process.decode_file ~in_filename:"dummy_file_encoded" ~out_filename:(Some "dummy_file2") with
  | Ok stats  -> Stats.print_stats stats
  | Error msg -> Printf.printf "Error : %s\n" msg
;;

test_decode () *)
