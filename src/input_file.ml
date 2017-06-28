open Nocrypto.Hash
open Sbx_version

type hash_state_and_chunks = SHA256.t * (bytes list)

type multihash_and_chunks  = bytes * (bytes list)

let file_hash_and_split ~(ver:version) ~(filename:string) : multihash_and_chunks =
  let read_block_size : int    = ver_to_data_size ver in
  let buf             : string = String.make read_block_size '\x00' in  (* Core.In_channel.input needs the buffer to be string instead of bytes as of time of writing - 2017-06-28 *)
  (* let buf             : bytes = Bytes.create read_block_size in *)

  let rec acc_chunks ~(acc:hash_state_and_chunks) ~(pos:int) ~(file:Core.In_channel.t) : hash_state_and_chunks =
    let (hash_state, chunks)       = acc in
    let read_count           : int = Core.In_channel.input file ~buf ~pos ~len:read_block_size in
    if read_count = 0 then
      acc
    else begin
      let new_chunk : bytes = Bytes.create read_count in
      String.blit buf 0 new_chunk 0 read_count;            (* copy content from buffer to new chunk*)
      SHA256.feed hash_state (Cstruct.of_bytes new_chunk); (* update hash state *)
      acc_chunks ~acc:(hash_state, (new_chunk :: chunks)) ~pos:(pos + read_count) ~file
    end in
  let acc_chunks_wrapper ~(file:Core.In_channel.t) : hash_state_and_chunks =
    acc_chunks ~acc:((SHA256.init ()), []) ~pos:0 ~file in

  let (final_hash_state, all_chunks) = Core.In_channel.with_file ~binary:true filename ~f:(fun file -> acc_chunks_wrapper ~file) in
  let hash_bytes                     = Bytes.of_string (Cstruct.to_string (SHA256.get final_hash_state)) in
  ((Multihash.raw_hash_to_multihash ~hash_type:`SHA256 ~raw:hash_bytes), all_chunks)
;;

