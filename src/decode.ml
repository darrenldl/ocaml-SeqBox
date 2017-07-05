(* Decoding workflow
 *  Go through entire file to scan for metadata block (seq_num = 0)
 *  using block size of 512(largest common divisor for implementable version 1, 3. Version 2 cannot be implemented)
 *    if metadata block exists
 *      all other data blocks must follow the metadata block's :
 *        version
 *        uid
 *    else
 *      first data block is used as guideline,
 *      all other data blocks must follow first data block's :
 *        version
 *        uid
 *
 *  when writing the blocks
 *    if CRC-CCITT fails
 *      drop the block
 *      do a printf
 *    else
 *      check if block of that seq_num has been written before via Hashtlbl (seq_num:uint32 -> unit)
 *        if no such block has been written yet
 *          write the block
 *        else
 *          drop the block
 *          do a printf
 *
 *  after writing all blocks
 *    if metadata block exists
 *      if outputfile exceeds file size
 *        use the file size recorded to trim the output file
 *      do a hash on the decoded file
 *        report if matches with the recorded hash if any
 *        leave it there even if it doesn't match
 *
 * statistics tracking
 *  block size used
 *  record no. blocks successfully decoded
 *  record no. blocks failed to decode
 *  record failing block positions
 *)
open Stdint
open Sbx_version
open Sbx_block
open Stream_file

let (<+>) = Int64.add;;

let (<->) = Int64.sub;;

let (<*>) = Int64.mul;;

type stats = { block_size       : int
             ; blocks_processed : int64
             ; blocks_decoded   : int64
             ; blocks_failed    : int64
             ; failed_block_pos : int64 list
             }

let make_blank_stats ~(ver:version) : stats =
  { block_size       = ver_to_block_size ver
  ; blocks_processed = 0L
  ; blocks_decoded   = 0L
  ; blocks_failed    = 0L
  ; failed_block_pos = []
  }

let add_okay_block ({ block_size; blocks_processed; blocks_decoded; blocks_failed; failed_block_pos }:stats) : stats =
  { block_size
  ; blocks_processed = blocks_processed <+> 1L
  ; blocks_decoded   = blocks_decoded   <+> 1L
  ; blocks_failed
  ; failed_block_pos
  }

let add_failed_block ({ block_size; blocks_processed; blocks_decoded; blocks_failed; failed_block_pos }:stats) : stats =
  { block_size
  ; blocks_processed = blocks_processed <+> 1L
  ; blocks_decoded
  ; blocks_failed    = blocks_failed    <+> 1L
  ; failed_block_pos = if blocks_failed < 500L then (blocks_processed <+> 1L) :: failed_block_pos else failed_block_pos
  }

let add_processed_block ({ block_size; blocks_processed; blocks_decoded; blocks_failed; failed_block_pos }:stats) : stats =
  { block_size
  ; blocks_processed = blocks_processed <+> 1L
  ; blocks_decoded
  ; blocks_failed
  ; failed_block_pos
  }

module Processor = struct
  let find_first_block_proc ~(want_meta:bool) (in_file:Core.In_channel.t) : Block.t option =
    let open Read_chunk in
    let len = 512 in (* largest common divisor for version 1, 3 block size *)
    let bytes_to_block (raw_header:Header.raw_header) (chunk:bytes) : Block.t option =
      let want_block =
        if want_meta then
          raw_header.seq_num =  (Uint32.of_int 0)
        else
          raw_header.seq_num != (Uint32.of_int 0) in
      if want_block then
        try
          (* the error has something to do with ~skipped_already maybe *)
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
          None
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
          | None       -> find_valid_data_block_proc_internal (add_failed_block stats) (* move onto finding next block *)
          | Some block ->
            begin
              if Block.is_meta block then (
                (* don't return metadata block *)
                find_valid_data_block_proc_internal (add_processed_block stats) (* move onto finding next block *) )
              else
                let file_uid = Block.block_to_file_uid block in
                let ver      = Block.block_to_ver      block in
                (* make sure uid and version match *)
                if file_uid = ref_file_uid && ver = ref_ver then
                  (add_okay_block stats, Some block)
                else
                  find_valid_data_block_proc_internal (add_failed_block stats) (* move onto finding next block *)
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
      let len       : int64 = Int64.of_int    (ver_to_block_size ref_ver) in
      match Block.block_to_seq_num block with
      | None         -> assert false
      | Some seq_num ->
        let seq_num   : int64 = Uint32.to_int64 seq_num in
        let write_pos : int64 = (seq_num <-> 1L) <*> len in  (* seq_num is guaranteed to be > 0 due to above check of is_meta *)
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
    decode_and_output_proc_internal (make_blank_stats ~ver:(Block.block_to_ver ref_block))
  ;;

  let decoder (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) : stats * (int64 option) =
    (* find a block to use as reference *)
    let ref_block : Block.t option =
      (* try to find a metadata block first *)
      match find_first_block_proc ~want_meta:true in_file with
      | Some block -> Some block
      | None       -> find_first_block_proc ~want_meta:false in_file (* get the first usable data block *) in
    match ref_block with
    | None           -> raise (Packaged_exn "no usable blocks in file")
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
      (stats, truncate)
  ;;
end

module Process = struct
  let decode_file ~(in_filename:string) ~(out_filename:string) : (stats, string) result =
    match Stream.process_in_out ~in_filename ~out_filename ~processor:Processor.decoder with
    | Ok (stats, Some trunc_size) ->
      begin
        try
          Unix.LargeFile.truncate out_filename trunc_size;
          Ok stats
        with
        | _ -> Error "failed to truncate output file"
      end
    | Ok (stats, None)            -> Ok stats
    | Error msg                   -> Error msg
  ;;
end

let test_decode () =
  let open Metadata in
  match Process.decode_file ~in_filename:"dummy_file_encoded" ~out_filename:"dummy_file2" with
  | Ok _      -> Printf.printf "Okay\n"
  | Error msg -> Printf.printf "Error : %s\n" msg
;;

test_decode ()
