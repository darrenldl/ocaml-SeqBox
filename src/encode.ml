open Stdint
open Sbx_version
open Sbx_block
open Stream_file
open Nocrypto.Hash

exception File_metadata_get_failed

type stats = { blocks_written : int64
             }

module Processor = struct
  (* Converts data to data blocks *)
  let rec data_to_block_proc ?(cur_block:int64 = 0L) (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) ~(len:int) ~(common:Header.common_fields) : stats =
    let open Read_chunk in
    let open Write_chunk in
    match read in_file ~len with
    | None           -> { blocks_written = cur_block }
    | Some { chunk } ->
      let block       = Block.make_data_block ~common ~data:chunk in
      let alt_seq_num = Uint32.of_int64 (Int64.add cur_block 1L) in (* always off by +1 *)
      let block_bytes = Block.to_bytes ~alt_seq_num block in
      write out_file ~chunk:block_bytes;
      data_to_block_proc ~cur_block:(Int64.add cur_block 1L) in_file out_file ~len ~common
  ;;

  let rec data_to_block_proc_w_hash ?(cur_block:int64 = 0L) ?(hash_state:SHA256.t = SHA256.init()) (in_file:Core.In_channel.t) (out_file:Core.Out_channel.t) ~(len:int) ~(common:Header.common_fields) : stats * bytes =
    let open Read_chunk in
    let open Write_chunk in
    match read in_file ~len with
    | None           -> ({ blocks_written = cur_block },
                         Conv_utils.sha256_hash_state_to_bytes hash_state)
    | Some { chunk } ->
      let block       = Block.make_data_block ~common ~data:chunk in
      let alt_seq_num = Uint32.of_int64 (Int64.add cur_block 1L) in (* always off by +1 *)
      let block_bytes = Block.to_bytes ~alt_seq_num block in
      (* update hash *)
      SHA256.feed hash_state (Cstruct.of_bytes chunk);
      (* write to file *)
      write out_file ~chunk:block_bytes;
      data_to_block_proc_w_hash ~cur_block:(Int64.add cur_block 1L) ~hash_state in_file out_file ~len ~common
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
         try
           (* write a empty metadata block first to shift space and also to test length of metadata fields *)
           let open Metadata in
           let fields_except_hash =
             List.filter (function | HSH _ -> false | _ -> true) metadata_list in
           let dummy_metadata_block       = Block.make_metadata_block ~common ~fields:fields_except_hash in
           let dummy_metadata_block_bytes = Block.to_bytes dummy_metadata_block in
           write out_file ~chunk:dummy_metadata_block_bytes;
           (* write data blocks *)
           let ({blocks_written}, hash)   = data_to_block_proc_w_hash in_file out_file ~len ~common in
           (* make the metadata block with hash *)
           let multihash                  = Multihash.raw_hash_to_multihash ~hash_type:`SHA256 ~raw:hash in
           let fields                     = (HSH multihash) :: fields_except_hash in
           let metadata_block             = Block.make_metadata_block ~common ~fields in
           let metadata_block_bytes       = Block.to_bytes metadata_block in
           (* go back and write metadata block *)
           Core.Out_channel.seek out_file 0L;
           write out_file ~chunk:metadata_block_bytes;
           (* update stats *)
           { blocks_written = Int64.add blocks_written 1L }
         with
         | Metadata.Too_much_data msg -> raise (Packaged_exn msg)
      )
  ;;
end

module Process = struct
  let get_file_metadata ~(in_filename:string) ~(out_filename:string) : Metadata.t list =
    try
      let open Metadata in
      let open File_utils in
      let open Time_utils in
      [ FNM in_filename
      ; SNM out_filename
      ; FSZ (getsize_uint64  ~filename:in_filename)
      ; FDT (getmtime_uint64 ~filename:in_filename)
      ; SDT (gettime_uint64 ())
      ]
    with
    | _ -> raise File_metadata_get_failed
  ;;

  let encode_file ~(uid:bytes option) ~(want_meta:bool) ~(in_filename:string) ~(out_filename:string) : (stats, string) result =
    try
      let common   =
        match uid with
        | Some uid -> Header.make_common_fields ~uid `V1
        | None     -> Header.make_common_fields      `V1 in
      let metadata =
        match want_meta with
        | true  -> Some (get_file_metadata ~in_filename ~out_filename)
        | false -> None in
      let encoder  = Processor.make_in_out_encoder ~common ~metadata in
      Stream.process_in_out ~in_filename ~out_filename ~processor:encoder
    with
    | Sbx_block.Header.Invalid_uid_length -> Error "Invalid uid length"
  ;;
end

let test_encode () =
  let open Metadata in
  match Process.encode_file ~uid:None ~want_meta:true ~in_filename:"dummy_file" ~out_filename:"dummy_file_encoded" with
  | Ok _      -> Printf.printf "Okay\n"
  | Error msg -> Printf.printf "Error : %s\n" msg
;;

test_encode ()
