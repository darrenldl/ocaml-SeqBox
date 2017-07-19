open Stdint

exception Length_mismatch
exception Unsupported_hash
exception Invalid_hash_type_string

type hash_type  = [ `SHA1
                  | `SHA2_256     | `SHA256
                  | `SHA2_512_256
                  | `SHA2_512_512 | `SHA512
                  | `BLAKE2B_256
                  | `BLAKE2B_512
                  | `BLAKE2S_128
                  | `BLAKE2S_256
                  ]

let all_hash_types = [ `SHA1
                     ; `SHA2_256     ; `SHA256
                     ; `SHA2_512_256
                     ; `SHA2_512_512 ; `SHA512
                     ; `BLAKE2B_256
                     ; `BLAKE2B_512
                     ; `BLAKE2S_128
                     ; `BLAKE2S_256
                     ]

type hash_bytes = hash_type * bytes

module Specs = struct
  type param = { hash_func_type : bytes
               ; digest_length  : int
               }

  let hash_type_to_param (hash_type:hash_type) : param =
    match hash_type with
    | `SHA1                   -> { hash_func_type = "\x11";   digest_length = 0x14 }
    | `SHA2_256     | `SHA256 -> { hash_func_type = "\x12";   digest_length = 0x20 }
    | `SHA2_512_256           -> { hash_func_type = "\x13";   digest_length = 0x20 }
    | `SHA2_512_512 | `SHA512 -> { hash_func_type = "\x13";   digest_length = 0x40 }
    | `BLAKE2B_256            -> { hash_func_type = "\xb220"; digest_length = 0x20 }
    | `BLAKE2B_512            -> { hash_func_type = "\xb240"; digest_length = 0x40 }
    | `BLAKE2S_128            -> { hash_func_type = "\xb250"; digest_length = 0x10 }
    | `BLAKE2S_256            -> { hash_func_type = "\xb260"; digest_length = 0x20 }
  ;;

  let hash_type_to_hash_func_type (hash_type:hash_type) : bytes =
    let { hash_func_type; _ } = hash_type_to_param hash_type in
    hash_func_type
  ;;

  let hash_type_to_digest_length  (hash_type:hash_type) : int =
    let { digest_length; _ } = hash_type_to_param hash_type in
    digest_length
  ;;

  let hash_type_to_total_length   (hash_type:hash_type) : int =
    let { hash_func_type; digest_length } = hash_type_to_param hash_type in
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

let hash_type_to_string (hash_type:hash_type) : string =
  match hash_type with
  | `SHA1                   -> "SHA1"
  | `SHA2_256     | `SHA256 -> "SHA256"
  | `SHA2_512_256           -> "SHA2-512"
  | `SHA2_512_512 | `SHA512 -> "SHA512"
  | `BLAKE2B_256            -> "BLAKE2B-256"
  | `BLAKE2B_512            -> "BLAKE2B-512"
  | `BLAKE2S_128            -> "BLAKE2S-128"
  | `BLAKE2S_256            -> "BLAKE2S-256"
;;

let string_to_hash_type_exn (str:string) : hash_type =
  match String.uppercase_ascii str with
  | "SHA1"         -> `SHA1
  | "SHA2-256"     -> `SHA2_256
  | "SHA256"       -> `SHA256
  | "SHA2-512-256" -> `SHA2_512_256
  | "SHA2-512-512" -> `SHA2_512_512
  | "SHA512"       -> `SHA512
  | "BLAKE2B-256"  -> `BLAKE2B_256
  | "BLAKE2B-512"  -> `BLAKE2B_512
  | "BLAKE2S-128"  -> `BLAKE2S_128
  | "BLAKE2S-256"  -> `BLAKE2S_256
  | _              -> raise Invalid_hash_type_string
;;

let string_to_hash_type (str:string) : (hash_type, string) result =
  try
    Ok (string_to_hash_type_exn str)
  with
  | Invalid_hash_type_string -> Error "Unrecognized hash type string"
;;

let raw_hash_to_hash_bytes (hash_type:hash_type) (raw:bytes) : hash_bytes =
  if Specs.hash_type_to_digest_length hash_type = Bytes.length raw then
    (hash_type, raw)
  else
    raise Length_mismatch
;;

let hash_bytes_to_raw_hash (hash_bytes:hash_bytes) : bytes =
  let (_, raw) = hash_bytes in
  raw
;;

let hash_bytes_to_multihash (hash_bytes:hash_bytes) : bytes =
  let (hash_type, raw)                        = hash_bytes in
  let { Specs.hash_func_type; digest_length } = Specs.hash_type_to_param hash_type in
  let len_bytes                               = Conv_utils.uint8_to_bytes (Uint8.of_int digest_length) in
  Bytes.concat "" [hash_func_type; len_bytes; raw]
;;

let hash_bytes_to_hash_type        (hash_bytes:hash_bytes) : hash_type =
  let (hash_type, _) = hash_bytes in
  hash_type
;;

let hash_bytes_to_hash_type_string (hash_bytes:hash_bytes) : bytes =
  let (hash_type, _) = hash_bytes in
  hash_type_to_string hash_type
;;

let raw_hash_to_multihash (hash_type:hash_type) (raw:bytes) : bytes =
  let hash_bytes = raw_hash_to_hash_bytes hash_type raw in
  hash_bytes_to_multihash hash_bytes
;;

let make_dummy_hash_bytes (hash_type:hash_type) : hash_bytes =
  (hash_type, Bytes.make (Specs.hash_type_to_digest_length hash_type) '\x00')
;;

module Parser = struct
  open Angstrom

  let gen_parser (hash_type:hash_type) : hash_bytes Angstrom.t =
    let { Specs.hash_func_type; digest_length } = Specs.hash_type_to_param hash_type in
    let len_bytes                               = Conv_utils.uint8_to_bytes (Uint8.of_int digest_length) in
    let header_bytes                            = Bytes.concat "" [hash_func_type; len_bytes] in
    string header_bytes *> take digest_length
    >>|
    (fun raw ->
       try
         raw_hash_to_hash_bytes hash_type raw
       with
       | Length_mismatch -> assert false
    )
  ;;

  let all_parsers : hash_bytes Angstrom.t list =
    List.map (fun hash_type -> gen_parser hash_type) all_hash_types
  ;;
end

module Hash = struct
  type ctx = SHA1    of Nocrypto.Hash.SHA1.t
           | SHA256  of Nocrypto.Hash.SHA256.t
           | SHA512  of Nocrypto.Hash.SHA512.t
           | BLAKE2B of Digestif.BLAKE2B.Bytes.ctx

  let ctx_to_hash_type (ctx:ctx) : hash_type =
    match ctx with
    | SHA1    _ -> `SHA1
    | SHA256  _ -> `SHA256
    | SHA512  _ -> `SHA512
    | BLAKE2B _ -> `BLAKE2B_512
  ;;

  let init (hash_type:hash_type) : ctx =
    match hash_type with
    | `SHA1                   -> SHA1    (Nocrypto.Hash.SHA1.init     ())
    | `SHA2_256     | `SHA256 -> SHA256  (Nocrypto.Hash.SHA256.init   ())
    | `SHA2_512_256           -> raise Unsupported_hash
    | `SHA2_512_512 | `SHA512 -> SHA512  (Nocrypto.Hash.SHA512.init   ())
    | `BLAKE2B_256            -> raise Unsupported_hash
    | `BLAKE2B_512            -> BLAKE2B (Digestif.BLAKE2B.Bytes.init ())
    | `BLAKE2S_128            -> raise Unsupported_hash
    | `BLAKE2S_256            -> raise Unsupported_hash
  ;;

  let hash_type_is_supported (hash_type:hash_type) : bool =
    try
      init hash_type |> ignore;
      true
    with
    | Unsupported_hash -> false
  ;;

  let feed (ctx:ctx) (data:bytes) : unit =
    let bytes_to_cstruct (bytes:bytes) : Cstruct.t =
      Cstruct.of_bytes bytes in
    match ctx with
    | SHA1    ctx -> Nocrypto.Hash.SHA1.feed     ctx (bytes_to_cstruct data)
    | SHA256  ctx -> Nocrypto.Hash.SHA256.feed   ctx (bytes_to_cstruct data)
    | SHA512  ctx -> Nocrypto.Hash.SHA512.feed   ctx (bytes_to_cstruct data)
    | BLAKE2B ctx -> Digestif.BLAKE2B.Bytes.feed ctx data
  ;;

  let get_raw_hash (ctx:ctx) : bytes =
    let cstruct_to_bytes (cstruct:Cstruct.t) : bytes =
      Cstruct.to_string cstruct in
    match ctx with
    | SHA1    ctx -> cstruct_to_bytes (Nocrypto.Hash.SHA1.get   ctx)
    | SHA256  ctx -> cstruct_to_bytes (Nocrypto.Hash.SHA256.get ctx)
    | SHA512  ctx -> cstruct_to_bytes (Nocrypto.Hash.SHA512.get ctx)
    | BLAKE2B ctx -> Digestif.BLAKE2B.Bytes.get ctx
  ;;

  let get_hash_bytes (ctx:ctx) : hash_bytes =
    let hash_type = ctx_to_hash_type ctx in
    let raw       = get_raw_hash ctx in
    raw_hash_to_hash_bytes hash_type raw
  ;;
end
