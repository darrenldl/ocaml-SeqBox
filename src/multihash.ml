open Exception

type hash = [ `SHA256 ]

let raw_hash_to_multihash ~(hash_type:hash) ~(raw:bytes) : bytes =
  match hash_type with
  | `SHA256 ->
    let len = Bytes.length raw in
    if len == 0x20 then
      let open Bytes in
      concat (create 0) [(of_string "\x12"); (of_string "\x20"); raw]
    else
      raise (Length_mismatch "length of raw hash provided does not match the hash length in specification")
;;
