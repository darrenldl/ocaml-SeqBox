open Stdint
open Sbx_version
open Sbx_block
open Stream_file
open Nocrypto.Hash

module Processor = struct
  type stats = { blocks_written : int
               }

  (* Converts data to data blocks *)
  let rec data_to_block_proc ?(cur_block:int = 0) (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) ~(len:int) ~(common:Header.common_fields) : stats =
    let open Read_chunk in
    let open Write_chunk in
    let {no_more_bytes; chunk} = read in_file ~len in
    let block                  = Block.make_data_block ~common ~data:chunk in
    let alt_seq_num            = Uint32.of_int (cur_block + 1) in (* always off by 1 *)
    let block_bytes            = Block.make_block_bytes ~alt_seq_num block in
    write out_file ~chunk:block_bytes;
    if no_more_bytes then
      { blocks_written = cur_block
      }
    else
      data_to_block_proc ~cur_block:(cur_block + 1) in_file out_file ~len ~common
  ;;

  let rec data_to_block_proc_w_hash ?(cur_block:int = 0) ?(hash_state:SHA256.t = SHA256.init()) (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) ~(len:int) ~(common:Header.common_fields) : stats * bytes =
    let open Read_chunk in
    let open Write_chunk in
    let {no_more_bytes; chunk} = read in_file ~len in
    let block                  = Block.make_data_block ~common ~data:chunk in
    let alt_seq_num            = Uint32.of_int (cur_block + 1) in (* always off by 1 *)
    let block_bytes            = Block.make_block_bytes ~alt_seq_num block in
    (* update hash *)
    SHA256.feed hash_state (Cstruct.of_bytes chunk);
    (* write to file *)
    write out_file ~chunk:block_bytes;
    if no_more_bytes then
      ({ blocks_written = cur_block
       },
       Conv_utils.sha256_hash_state_to_bytes hash_state
      )
    else
      data_to_block_proc_w_hash ~cur_block:(cur_block + 1) ~hash_state in_file out_file ~len ~common
  ;;

  let make_in_out_encoder ~(common:Header.common_fields) ~(metadata:(Metadata.t list) option) : stats Stream.in_out_processor =
    let ver = Header.common_fields_to_ver common in
    let len = ver_to_data_size ver in
    let open Read_chunk in
    let open Write_chunk in
    match metadata with
    | None ->
      (fun in_file out_file ->
         data_to_block_proc in_file out_file ~len ~common
      )
    | Some metadata_list ->
      (fun in_file out_file ->
         (* write a dummy metadata block first *)
         let dummy_block_bytes = General_helper.make_buffer len in
         write out_file ~chunk:dummy_block_bytes;
         (* write data blocks *)
         let ({blocks_written}, hash) = data_to_block_proc_w_hash in_file out_file ~len ~common in
         (* make the metadata block with hash *)
         let open Metadata in
         let fields_except_hash =
           List.filter (function | HSH _ -> false | _ -> true) metadata_list in
         let fields = (HSH hash) :: fields_except_hash in
         (* go back and write metadata block *)
         let metadata_block       = Block.make_metadata_block ~common ~fields in
         let metadata_block_bytes = Block.make_block_bytes metadata_block in
         Core.Out_channel.seek out_file 0L;
         write out_file ~chunk:metadata_block_bytes;
         (* update stats *)
         { blocks_written = blocks_written + 1 }
      )
  ;;
end

let test_encode () =
  let open Metadata in
  let common   = Header.make_common_fields `V1 in
  let metadata = Some [ FNM "filename"
                      ; SNM "filename.sbx"
                      ; FDT (Uint64.of_int 1000000)
                      ; SDT (Uint64.of_int 1000001)
                      ] in
  let encoder = Processor.make_in_out_encoder ~common ~metadata in
  encoder
;;
