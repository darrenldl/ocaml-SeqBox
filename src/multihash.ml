exception Length_mismatch

type hash = [ `SHA256 ]

let raw_hash_to_multihash ~(hash_type:hash) ~(raw:bytes) : bytes =
  match hash_type with
  | `SHA256 ->
    let len = Bytes.length raw in
    if len == 0x20 then
      let open Bytes in
      concat (create 0) [(of_string "\x12"); (of_string "\x20"); raw]
    else
      raise Length_mismatch
;;

let make_dummy_multihash ~(hash_type:hash) : bytes =
  match hash_type with
  | `SHA256 ->
    let open Bytes in
    concat (create 0) [(of_string "\x12"); (of_string "\x20"); (Bytes.make 0x20 '\x00')]
;;
