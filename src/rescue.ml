open Stream_file
open Sbx_block

let (<+>) = Int64.add;;

let (<->) = Int64.sub;;

let (<*>) = Int64.mul;;

let (</>) = Int64.div;;

module Stats = struct
  type t = { bytes_processed       : int64
           ; blocks_processed      : int64
           ; meta_blocks_processed : int64
           ; data_blocks_processed : int64
           }

  let make_blank_stats () : t =
    { bytes_processed       = 0L
    ; blocks_processed      = 0L
    ; meta_blocks_processed = 0L
    ; data_blocks_processed = 0L
    }
  ;;

  let add_bytes (stats:t) ~(num:int64) : t =
    { bytes_processed       = stats.bytes_processed       <+> num
    ; blocks_processed      = stats.blocks_processed
    ; meta_blocks_processed = stats.meta_blocks_processed
    ; data_blocks_processed = stats.data_blocks_processed
    }
  ;;

  let add_meta_block (stats:t) : t =
    { bytes_processed       = stats.bytes_processed
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_processed = stats.meta_blocks_processed <+> 1L
    ; data_blocks_processed = stats.data_blocks_processed
    }
  ;;

  let add_data_block (stats:t) : t =
    { bytes_processed       = stats.bytes_processed
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_processed = stats.meta_blocks_processed
    ; data_blocks_processed = stats.data_blocks_processed <+> 1L
    }
  ;;

  (* automatically correct bytes_processed alignment
   * by rounding to closest 128 bytes
   *)
  let make_stats (bytes_processed:int64) (blocks_processed:int64) (meta_blocks_processed:int64) (data_blocks_processed:int64) : t =
    { bytes_processed =
        begin
          let alignment = Int64.of_int Param.Rescue.scan_alignment in
          (bytes_processed </> alignment ) <*> alignment
        end
    ; blocks_processed
    ; meta_blocks_processed
    ; data_blocks_processed
    }
  ;;

  let print_stats_single_line (stats:t) : unit =
    Printf.printf "\rBytes : %Ld, Blocks : %Ld, Meta : %Ld, Data : %Ld"
      stats.bytes_processed
      stats.blocks_processed
      stats.meta_blocks_processed
      stats.data_blocks_processed
  ;;

  let print_stats (stats:t) : unit =
    Printf.printf "Number of          bytes  processed : %Ld\n" stats.bytes_processed;
    Printf.printf "Number of          blocks processed : %Ld\n" stats.blocks_processed;
    Printf.printf "Number of metadata blocks processed : %Ld\n" stats.meta_blocks_processed;
    Printf.printf "Number of data     blocks processed : %Ld\n" stats.data_blocks_processed
  ;;
end

type stats = Stats.t

module Logger = struct
  let make_write_proc ~(stats:stats) : unit Stream.out_processor =
    (fun out_file ->
       let open Write_chunk in
       write out_file ~chunk:(Printf.sprintf "bytes_processed=%Ld\n"       stats.bytes_processed);
       write out_file ~chunk:(Printf.sprintf "blocks_processed=%Ld\n"      stats.blocks_processed);
       write out_file ~chunk:(Printf.sprintf "meta_blocks_processed=%Ld\n" stats.meta_blocks_processed);
       write out_file ~chunk:(Printf.sprintf "data_blocks_processed=%Ld\n" stats.data_blocks_processed);
    )
  ;;

  let write_log ~(stats:stats) ~(log_filename:string) : (unit, string) result =
    let processor = make_write_proc ~stats in
    match Stream.process_out ~append:false ~out_filename:log_filename ~processor with
    | Ok _      -> Ok ()
    | Error msg -> Error msg
  ;;

  module Parser = struct
    open Angstrom
    open Parser_components

    let bytes_processed_p =
      string "bytes_processed=" *> integer64 <* string "\n"
    ;;

    let blocks_processed_p =
      string "blocks_processed=" *> integer64 <* string "\n"
    ;;

    let meta_blocks_processed_p =
      string "meta_blocks_processed=" *> integer64 <* string "\n"
    ;;

    let data_blocks_processed_p =
      string "data_blocks_processed=" *> integer64 <* string "\n"
    ;;

    let log_file_p =
      lift4 Stats.make_stats bytes_processed_p blocks_processed_p meta_blocks_processed_p data_blocks_processed_p
    ;;
  end

  let make_read_proc () : (stats option) Stream.in_processor =
    let open Read_chunk in
    let try_parse_len = 1000  (* grab some bytes and try to parse, ignore rest of the log file *) in
    (fun in_file ->
       match read in_file ~len:try_parse_len with
       | None         -> None
       | Some { chunk } ->
         let open Angstrom in
         match parse_only Parser.log_file_p (`String chunk) with
         | Ok stats -> Some stats
         | Error _  -> None
    )
  ;;

  let read_log ~(log_filename:string) : (stats option, string) result =
    let processor = make_read_proc () in
    if Sys.file_exists log_filename then
      match Stream.process_in ~in_filename:log_filename ~processor with
      | Ok v      -> Ok v
      | Error msg -> Error msg
    else
      Ok (Some (Stats.make_blank_stats ()))
  ;;
end

module Processor = struct
  (* scan for valid block *)
  let scan_proc ~(stats:stats) (in_file:Core.In_channel.t) : stats * ((Block.t * bytes) option) =
    let open Read_chunk in
    let len = Param.Rescue.scan_alignment in
    let bytes_to_block (raw_header:Header.raw_header) (chunk:bytes) : Block.t option =
      try
        Some (Block.of_bytes ~raw_header chunk)
      with
        | Header.Invalid_bytes
        | Metadata.Invalid_bytes
        | Block.Invalid_bytes
        | Block.Invalid_size     -> None in
    let rec scan_proc_internal (stats:stats) : stats * ((Block.t * bytes) option) =
      match read in_file ~len with
      | None           -> (stats, None)
      | Some { chunk } ->
        let new_stats =
          Stats.add_bytes stats ~num:(Int64.of_int (Bytes.length chunk)) in
        if Bytes.length chunk < 16 then
          (new_stats, None)  (* no more bytes left in file *)
        else
          let test_header_bytes = Misc_utils.get_bytes chunk ~pos:0 ~len:16 in
          let test_header : Header.raw_header option =
            try
              Some (Header.of_bytes test_header_bytes)
            with
            | Header.Invalid_bytes -> None in
          match test_header with
          | None            -> scan_proc_internal new_stats
          | Some raw_header ->
            (* possibly grab more bytes depending on version *)
            let chunk =
              Processor_helpers.patch_block_bytes_if_needed in_file ~raw_header ~chunk in
            let test_block : Block.t option =
              bytes_to_block raw_header chunk in
            let new_stats =
              Stats.add_bytes stats ~num:(Int64.of_int (Bytes.length chunk)) in
            match test_block with
            | None       -> scan_proc_internal new_stats
            | Some block -> (new_stats, Some (block, chunk))  (* found a valid block *) in
    scan_proc_internal stats
  ;;

  (* append blocks to filename (use uid in hex string as filename)
   * return Error if failed to write for whatever reason
   *)
  let output_proc ~(stats:stats) ~(block_and_chunk:Block.t * bytes) ~(out_dirname:string) : stats * ((unit, string) result) =
    let (block, chunk) = block_and_chunk in
    let out_filename =
      let uid_hex =
        Conv_utils.bytes_to_hex_string (Block.block_to_file_uid block) in
      let separator =
        if String.get out_dirname ((String.length out_dirname) - 1) = '/' then
          ""
        else
          "/" in
      String.concat separator [out_dirname; uid_hex] in
    let output_proc_internal_processor (out_file:Core.Out_channel.t) : unit =
      let open Write_chunk in
      (* Core.Out_channel.seek out_file (Int64.sub (Core.Out_channel.length out_file) 1L); (* append to file *) *)
      write out_file ~chunk in  (* use the actual bytes in original file rather than generating from scratch *)
    let new_stats =
      if Block.is_meta block then
        Stats.add_meta_block stats
      else
        Stats.add_data_block stats in
    match Stream.process_out ~append:true ~out_filename ~processor:output_proc_internal_processor with
    | Ok _      -> (new_stats, Ok ())
    | Error msg -> (new_stats, Error msg)
  ;;

  (* if there is any error with outputting, just print directly and return stats
   * this should be very rare however, if happening at all
   *)
  let rec scan_and_output ~(stats:stats) ~(out_dirname:string) ~(log_filename:string option) (in_file:Core.In_channel.t) : stats =
    (* exit if failed to write to log
     *
     * print a new line before exitting to not print on the same line as the stats
     *)
    let log_okay : bool =
      match log_filename with
      | None              -> true
      | Some log_filename ->
        match Logger.write_log ~stats ~log_filename with
        | Error msg -> print_newline (); Printf.printf "%s" msg; print_newline (); false
        | Ok _      -> true in
    if not log_okay then
      stats
    else
      begin
        (* report progress *)
        Stats.print_stats_single_line stats;
        match scan_proc ~stats in_file with
        | (stats, None)                 -> print_newline (); stats  (* ran out of valid blocks in input file *)
        | (stats, Some block_and_chunk) ->
          match output_proc ~stats ~block_and_chunk ~out_dirname with
          | (stats, Ok _ )     -> scan_and_output ~stats ~out_dirname ~log_filename in_file
          | (stats, Error msg) -> print_newline (); Printf.printf "%s" msg; print_newline (); stats
      end
  ;;

  let make_rescuer ~(out_dirname:string) ~(log_filename:string option) : ((stats, string) result) Stream.in_processor =
    (fun in_file ->
       (* try to get last stats from log file and seek to the last position recorded
        * otherwise just make blank stats
        *)
       let possibly_stats : (stats, string) result  =
         match log_filename with
         | None      -> Ok (Stats.make_blank_stats ())
         | Some log_filename ->
           match Logger.read_log ~log_filename with
           | Error msg       -> Error msg
           | Ok None         -> Error "Failed to parse log file"
           | Ok (Some stats) -> Ok stats in
       match possibly_stats with
       | Error msg -> Error msg (* just exit due to error *)
       | Ok stats  ->
         (* seek to last position read *)
         Core.In_channel.seek in_file stats.bytes_processed;
         (* start scan and output process *)
         Ok (scan_and_output in_file ~stats ~out_dirname ~log_filename)
    )
  ;;

end

module Process = struct
  let rescue_from_file ~(in_filename:string) ~(out_dirname:string) ~(log_filename:string option) : (stats, string) result =
    let processor = Processor.make_rescuer ~out_dirname ~log_filename in
    match Stream.process_in ~in_filename ~processor with
    | Ok stats  -> stats
    | Error msg -> Error msg
  ;;
end

(* let test () =
  match Process.rescue_from_file ~in_filename:"dummy_disk" ~out_dirname:"rescue_folder" ~log_filename:(Some "rescue_log") with
  | Ok stats  -> Stats.print_stats stats
  | Error msg -> Printf.printf "%s\n" msg
;;

test () *)
