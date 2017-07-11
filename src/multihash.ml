exception Length_mismatch

type hash_type  = [ `SHA256 ]

type hash_bytes = hash_type * bytes

module Specs = struct
  let hash_length ~(hash_type:hash_type) =
    match hash_type with
    | `SHA256 -> 0x20
  ;;
end

let raw_hash_to_hash_bytes ~(hash_type:hash_type) ~(raw:bytes) : hash_bytes =
  if Specs.hash_length ~hash_type = Bytes.length raw then
    (hash_type, raw)
  else
    raise Length_mismatch
;;

let hash_bytes_to_raw_hash ~(hash_bytes:hash_bytes) : bytes =
  match hash_bytes with
  | (_, raw) -> raw
;;

let hash_bytes_to_multihash ~(hash_bytes:hash_bytes) : bytes =
  match hash_bytes with
  | (`SHA256, raw) ->
    let open Bytes in
    concat (create 0) [(of_string "\x12"); (of_string "\x20"); raw]
;;

let raw_hash_to_multihash ~(hash_type:hash_type) ~(raw:bytes) : bytes =
  let hash_bytes = raw_hash_to_hash_bytes ~hash_type ~raw in
  hash_bytes_to_multihash ~hash_bytes
;;

let make_dummy_hash_bytes ~(hash_type:hash_type) : hash_bytes =
  match hash_type with
  | `SHA256 -> (`SHA256, Bytes.make 0x20 '\x00')
;;

module Parser = struct
  open Angstrom

  let sha256_p =
    string "\x22" *> string "\x12\x20" *> take (Specs.hash_length `SHA256)
    >>|
    (fun raw ->
       try
         raw_hash_to_hash_bytes ~hash_type:`SHA256 ~raw
       with
       | Length_mismatch -> assert false
    )
  ;;
end
