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
 *)
open Stdint
open Sbx_version
open Sbx_block
open Stream_file

type stats = { blocks_decoded : int
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
      if want_block then (
        try
          (* the error has something to do with ~skipped_already maybe *)
          Some (Block.of_bytes ~raw_header chunk)
        with
        | Header.Invalid_bytes
        | Metadata.Invalid_bytes
        | Block.Invalid_bytes
        | Block.Invalid_size     -> None )
      else (
        None )in
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
  let find_valid_data_block_proc ~(ref_block:Block.t) (in_file:Core.In_channel.t) : Block.t option =
    let open Read_chunk in
    let ref_ver      = Block.block_to_ver ref_block in
    let len          = ver_to_block_size ref_ver in
    let ref_file_uid = Block.block_to_file_uid ref_block in
    let rec find_valid_data_block_proc_internal () : Block.t option =
        match read in_file ~len with
        | None           -> None
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
          | None       -> find_valid_data_block_proc_internal () (* move onto finding next block *)
          | Some block ->
            begin
              if Block.is_meta block then (
                (* don't return metadata block *)
                find_valid_data_block_proc_internal () (* move onto finding next block *) )
              else (
                let file_uid = Block.block_to_file_uid block in
                let ver      = Block.block_to_ver      block in
                (* make sure uid and version matches *)
                if file_uid = ref_file_uid && ver = ref_ver then
                  Some block
                else
                  find_valid_data_block_proc_internal () (* move onto finding next block *) )
            end in
    find_valid_data_block_proc_internal ()
  ;;

  let output_decoded_data_proc ~(block:Block.t) (out_file:Core.Out_channel.t) : unit =
    let open Write_chunk in
    write out_file ~chunk:(Block.block_to_data block)
  ;;

  let decode_and_output_proc ~(ref_block:Block.t) (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) : stats =
    let rec decode_and_output_proc_internal ({blocks_decoded; _}:stats) : stats =
      match find_valid_data_block_proc ~ref_block in_file with
      | None       -> {blocks_decoded}
      | Some block ->
        output_decoded_data_proc ~block out_file;
        decode_and_output_proc_internal {blocks_decoded = blocks_decoded + 1} in
    decode_and_output_proc_internal {blocks_decoded = 0}
  ;;

  let decoder (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) : stats =
    let ref_block : Block.t option =
      (* try to find a metadata block first *)
      match find_first_block_proc ~want_meta:true in_file with
      | Some block -> Some block
      | None       -> find_first_block_proc ~want_meta:false in_file (* get the first usable data block *) in
    match ref_block with
    | None           -> raise (Packaged_exn "no usable blocks in file")
    | Some ref_block -> decode_and_output_proc ~ref_block in_file out_file
  ;;
end

module Process = struct
  let decode_file ~(in_filename:string) ~(out_filename:string) : (stats, string) result =
    Stream.process_in_out ~in_filename ~out_filename ~processor:Processor.decoder
  ;;
end

let test_decode () =
  let open Metadata in
  match Process.decode_file ~in_filename:"dummy_file_encoded" ~out_filename:"dummy_file2" with
  | Ok _      -> Printf.printf "Okay\n"
  | Error msg -> Printf.printf "Error : %s\n" msg
;;

test_decode ()
