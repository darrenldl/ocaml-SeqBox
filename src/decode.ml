(* Decoding workflow
 *  Go through entire file to scan for metadata block (seq_num = 0)
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
