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
  let find_metadata_block_proc (in_file:Core.In_channel.t) : Block.t option =
    let open Read_chunk in
    let len = 512 in  (* largest common divisor for version 1, 3 block size *)
    let rec find_metadata_block_proc_internal () : Block.t option =
      let {no_more_bytes; chunk} = read in_file ~len in
      if no_more_bytes || Bytes.length chunk < 16 then
        None  (* at the end of file and/or got nothing *)
      else
        let test_header_bytes = Misc_utils.get_bytes chunk ~pos:0 ~len:16 in
        let test_header : Header.raw_header option =
          try
            Some (Header.of_bytes test_header_bytes)
          with
          | Header.Invalid_bytes -> None in
        match test_header with
        | None        ->
          find_metadata_block_proc_internal () (* go to next block *)
        | Some raw_header ->
          let test_block : Block.t option =
            if raw_header.seq_num = (Uint32.of_int 0) then
              (* may have got a metadata block *)
              try
                Some (Block.of_bytes ~raw_header chunk)
              with
              | Header.Invalid_bytes   -> None
              | Metadata.Invalid_bytes -> None
              | Block.Invalid_bytes    -> None
            else
              None in
          match test_block with
          | None -> 
            find_metadata_block_proc_internal () (* go to next block *)
          | Some block ->
            Some block  (* found a valid block *) in
    let res = find_metadata_block_proc_internal () in
    Core.In_channel.seek in_file 0L;  (* reset seek position *)
    res
  ;;

  (* ref_block will be used as reference for version and uid
   *  block must match those two parameters to be accepted
   *)
  let find_valid_data_block_proc ~(ref_block:Block.t) (in_file:Core.In_channel.t) : Block.t option =
    let open Read_chunk in
    let ver = Block.block_to_ver ref_block in
    let len = ver_to_block_size ver in
    let rec find_valid_data_block_proc_internal () =
      let {no_more_bytes; chunk} = read in_file ~len in
      let block =
        try
          Some (Block.of_bytes chunk)
        with
        | Header.Invalid_bytes   -> None
        | Metadata.Invalid_bytes -> None
        | Block.Invalid_bytes    -> None in
      match block with
      | Some block ->
        if Block.is_meta block then
          (* don't return metadata block *)
          find_valid_data_block_proc_internal () (* move onto finding next block *)
        else
          Some block
      | None       -> find_valid_data_block_proc_internal () (* move onto finding next block *) in
    find_valid_data_block_proc_internal ()
  ;;

  let output_decoded_data_proc_if_any ~(block:Block.t option) (out_file:Core.Out_channel.t) : unit =
    let open Write_chunk in
    match block with
    | Some block ->
      write out_file ~chunk:(Block.block_to_data block)
    | None -> ()
  ;;

  (*let decoder (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) : stats =*)
end
