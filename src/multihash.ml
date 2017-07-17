open Stdint

exception Length_mismatch

type hash_type  = [ `SHA1
                  | `SHA2_256     | `SHA256
                  | `SHA2_512_256
                  | `SHA2_512_512 | `SHA512
                  | `BLAKE2B_256
                  | `BLAKE2B_512
                  | `BLAKE2S_128
                  | `BLAKE2S_256
                  ]

type hash_bytes = hash_type * bytes

module Specs = struct
  type param = { hash_func_type : bytes
               ; digest_length  : int
               }

  let hash_type_to_param ~(hash_type:hash_type) : param =
    match hash_type with
    | `SHA1         -> { hash_func_type = "\x11";   digest_length = 0x14 }
    | `SHA2_256
    | `SHA256       -> { hash_func_type = "\x12";   digest_length = 0x20 }
    | `SHA2_512_256 -> { hash_func_type = "\x13";   digest_length = 0x20 }
    | `SHA2_512_512
    | `SHA512       -> { hash_func_type = "\x13";   digest_length = 0x40 }
    | `BLAKE2B_256  -> { hash_func_type = "\xb220"; digest_length = 0x20 }
    | `BLAKE2B_512  -> { hash_func_type = "\xb240"; digest_length = 0x40 }
    | `BLAKE2S_128  -> { hash_func_type = "\xb250"; digest_length = 0x10 }
    | `BLAKE2S_256  -> { hash_func_type = "\xb260"; digest_length = 0x20 }
  ;;

  let hash_type_to_hash_func_type ~(hash_type:hash_type) : bytes =
    let { hash_func_type; _ } = hash_type_to_param ~hash_type in
    hash_func_type
  ;;

  let hash_type_to_digest_length  ~(hash_type:hash_type) : int =
    let { digest_length; _ } = hash_type_to_param ~hash_type in
    digest_length
  ;;

  let hash_type_to_total_length   ~(hash_type:hash_type) : int =
    let { hash_func_type; digest_length } = hash_type_to_param ~hash_type in
    (Bytes.length hash_func_type) + 1 + digest_length
  ;;
end

let hash_bytes_equal (a:hash_bytes) (b:hash_bytes) : bool =
  let (hash_type_a, raw_a) = a in
  let (hash_type_b, raw_b) = b in
  if hash_type_a = hash_type_b then
    (Bytes.compare raw_a raw_b) = 0
  else
    false
;;

let hash_type_to_string ~(hash_type:hash_type) : string =
  match hash_type with
  | `SHA1         -> "SHA1"
  | `SHA2_256
  | `SHA256       -> "SHA256"
  | `SHA2_512_256 -> "SHA2-512"
  | `SHA2_512_512
  | `SHA512       -> "SHA512"
  | `BLAKE2B_256  -> "BLAKE2B-256"
  | `BLAKE2B_512  -> "BLAKE2B-512"
  | `BLAKE2S_128  -> "BLAKE2S-128"
  | `BLAKE2S_256  -> "BLAKE2S-256"
;;

let raw_hash_to_hash_bytes ~(hash_type:hash_type) ~(raw:bytes) : hash_bytes =
  if Specs.hash_type_to_digest_length ~hash_type = Bytes.length raw then
    (hash_type, raw)
  else
    raise Length_mismatch
;;

let hash_bytes_to_raw_hash ~(hash_bytes:hash_bytes) : bytes =
  let (_, raw) = hash_bytes in
  raw
;;

let hash_bytes_to_multihash ~(hash_bytes:hash_bytes) : bytes =
  let (hash_type, raw)                        = hash_bytes in
  let { Specs.hash_func_type; digest_length } = Specs.hash_type_to_param ~hash_type in
  let len_bytes                               = Conv_utils.uint8_to_bytes (Uint8.of_int digest_length) in
  Bytes.concat "" [hash_func_type; len_bytes; raw]
;;

let hash_bytes_to_hash_type        ~(hash_bytes:hash_bytes) : hash_type =
  let (hash_type, _) = hash_bytes in
  hash_type
;;

let hash_bytes_to_hash_type_string ~(hash_bytes:hash_bytes) : bytes =
  let (hash_type, _) = hash_bytes in
  hash_type_to_string ~hash_type
;;

let raw_hash_to_multihash ~(hash_type:hash_type) ~(raw:bytes) : bytes =
  let hash_bytes = raw_hash_to_hash_bytes ~hash_type ~raw in
  hash_bytes_to_multihash ~hash_bytes
;;

let make_dummy_hash_bytes ~(hash_type:hash_type) : hash_bytes =
  (hash_type, Bytes.make (Specs.hash_type_to_digest_length hash_type) '\x00')
;;

module Parser = struct
  open Angstrom

  let gen_parser ~(hash_type:hash_type) : hash_bytes Angstrom.t =
    let { Specs.hash_func_type; digest_length } = Specs.hash_type_to_param hash_type in
    let len_bytes                               = Conv_utils.uint8_to_bytes (Uint8.of_int digest_length) in
    let header_bytes                            = Bytes.concat "" [hash_func_type; len_bytes] in
    string header_bytes *> take digest_length
    >>|
    (fun raw ->
       try
         raw_hash_to_hash_bytes ~hash_type ~raw
       with
       | Length_mismatch -> assert false
    )
  ;;
end
