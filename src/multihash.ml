
exception Length_mismatch

type hash = [ `SHA256 ]

module Parser = struct
  open Angstrom

  let sha256_p =
    string "\x22" *> string "\x12\x20" *> take 0x20
  ;;
end

let raw_hash_to_multihash ~(hash_type:hash) ~(raw:bytes) : bytes =
  match hash_type with
  | `SHA256 ->
    let len = Bytes.length raw in
    if len = 0x20 then
      let open Bytes in
      concat (create 0) [(of_string "\x12"); (of_string "\x20"); raw]
    else
      raise Length_mismatch
;;

let make_dummy_raw_hash ~(hash_type:hash) : bytes =
  match hash_type with
  | `SHA256 -> Bytes.make 0x20 '\x00'
;;
