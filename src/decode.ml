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
 *    if CRC-CCITT fails, drop the block
 *
 *  after writing all blocks
 *    if metadata block exists
 *      if outputfile exceeds file size
 *        use the file size recorded to trim the output file
 *)
