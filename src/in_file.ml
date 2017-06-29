open Core
open Nocrypto.Hash
open Sbx_version

module Raw_file = struct
  type chunks                = string list

  type hash_state_and_chunks = SHA256.t * chunks

  type multihash_and_chunks  = string   * chunks

  let file_hash_and_split ~(ver:version) ~(filename:string) : (multihash_and_chunks, string) result =
    let read_block_size : int    = ver_to_data_size ver in
    let buf             : string = String.make read_block_size '\x00' in  (* Core.In_channel.input needs the buffer to be string instead of bytes as of time of writing - 2017-06-28 *)
    (* let buf             : bytes = Bytes.create read_block_size in *)

    let acc_chunks ~(file:Core.In_channel.t) : (hash_state_and_chunks, string) result =
      let rec acc_chunks_helper ~(acc:hash_state_and_chunks) ~(file:In_channel.t) : hash_state_and_chunks =
        let (hash_state, chunks)       = acc in
        let read_count           : int = In_channel.input        file ~buf ~pos:0 ~len:read_block_size in
        let new_chunk                  = Bytes.create read_count in
        String.blit ~src:buf ~src_pos:0 ~dst:new_chunk ~dst_pos:0 ~len:read_count; (* copy content from buffer to new chunk*)
        SHA256.feed hash_state (Cstruct.of_bytes new_chunk);                       (* update hash state *)
        let new_hash_and_chunks        = (hash_state, if read_count > 0 then (new_chunk :: chunks) else chunks ) in
        if read_count < read_block_size then
          new_hash_and_chunks
        else begin
          acc_chunks_helper ~acc:new_hash_and_chunks ~file
        end in
      try
        Ok (acc_chunks_helper ~acc:((SHA256.init ()), []) ~file)
      with
      | _ -> Error (Printf.sprintf "Failed to read file : %s" filename) in

    try
      match In_channel.with_file ~binary:true filename ~f:(fun file -> acc_chunks ~file) with
      | Ok (final_hash_state, all_chunks) ->
        let hash_bytes = Cstruct.to_string (SHA256.get final_hash_state) in
        Ok ((Multihash.raw_hash_to_multihash ~hash_type:`SHA256 ~raw:hash_bytes), all_chunks)
      | Error msg -> Error msg
    with
    | Sys_error msg -> Error msg
  ;;

  let file_split ~(ver:version) ~(filename:string) : (chunks, string) result =
    let read_block_size : int    = ver_to_block_size ver in
    let buf             : string = String.make read_block_size '\x00' in

    let acc_chunks ~(file:Core.In_channel.t) : (chunks, string) result =
      let rec acc_chunks_helper ~(acc:chunks) ~(file:In_channel.t) : chunks =
        let read_count           : int = In_channel.input        file ~buf ~pos:0 ~len:read_block_size in
        let new_chunk                  = Bytes.create read_count in
        String.blit ~src:buf ~src_pos:0 ~dst:new_chunk ~dst_pos:0 ~len:read_count; (* copy content from buffer to new chunk*)
        let new_chunks                 = if read_count > 0 then (new_chunk :: acc) else acc in
        if read_count < read_block_size then
          new_chunks
        else begin
          acc_chunks_helper ~acc:new_chunks ~file
        end in
      try
        Ok (acc_chunks_helper ~acc:[] ~file)
      with
      | _ -> Error (Printf.sprintf "Failed to read file : %s" filename) in

    try
      In_channel.with_file ~binary:true filename ~f:(fun file -> acc_chunks ~file)
    with
    | Sys_error msg -> Error msg
  ;; 
end

module Sbx_file = struct

end

let test () : unit =
  match Raw_file.file_hash_and_split ~ver:`V1 ~filename:"dummy_file" with
  | Ok (hash, chunks) ->
    Printf.printf "multihash        : %s\n" (let (`Hex hex_str) = (Hex.of_string hash) in hex_str);
    Printf.printf "number of chunks : %d\n" (List.length chunks)
  | Error msg ->
    Printf.printf "Got and error    : %s\n" msg
;;

test ();;
