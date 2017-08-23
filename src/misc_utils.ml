open Stdint

exception Invalid_range

let pad_bytes ?(filler:uint8 = Uint8.of_int 0x00) (old_bytes:bytes) (new_len:int) : bytes =
  let buf         = Bytes.create 1 in
  Uint8.to_bytes_big_endian filler buf 0;
  let filler_char = Bytes.get buf 0 in
  let old_len     = Bytes.length old_bytes in
  if old_len < new_len then
    let new_bytes = Bytes.make new_len filler_char in
    Bytes.blit old_bytes 0 new_bytes 0 old_len;
    new_bytes
  else
    old_bytes
;;

let get_bytes (chunk:bytes) ~(pos:int) ~(len:int) : bytes =
  let chunk_size = Bytes.length chunk in
  if      pos < 0 || pos >= chunk_size then
    raise Invalid_range
  else if len < 0 then
    raise Invalid_range
  else if pos + (len - 1) >= chunk_size then
    raise Invalid_range
  else
    Bytes.sub chunk pos len
;;

let get_bytes_inc_range (chunk:bytes) ~(start_at:int) ~(end_at:int) : bytes =
  get_bytes chunk ~pos:start_at ~len:(end_at     - start_at + 1)
;;

let get_bytes_exc_range (chunk:bytes) ~(start_at:int) ~(end_before:int) : bytes =
  get_bytes chunk ~pos:start_at ~len:(end_before - start_at)
;;

let list_find_option (pred:('a -> bool)) (lst:'a list) : 'a option =
  try
    Some (List.find pred lst)
  with
  | Not_found -> None
;;

let make_path (path_parts:string list) : string =
  let strip_slash str =
    let str_len = String.length str in
    match str_len with
    | 0 -> str
    | 1 ->
      begin
        if (String.get str 0) = '/' then "" else str
      end
    | _ ->
      begin
        let char_last     = String.get str (str_len - 1) in
        let char_2nd_last = String.get str (str_len - 2) in
        if char_last = '/' && char_2nd_last <> '\\' then
          get_bytes str ~pos:0 ~len:(str_len - 1)
        else
          str
      end in
  let lst = List.map strip_slash path_parts in
  String.concat "/" lst
;;

let char_list_to_string (lst:char list) : string =
  String.concat "" (List.map (fun c -> String.make 1 c) lst)
;;

let path_to_list (path:string) : string list =
  let open Angstrom in
  let sep           : unit Angstrom.t =
    string "/" *> return () in
  let escaped_sep   : char list Angstrom.t =
    string "\\/" *> return ['\\'; '/'] in
  let not_sep       : char list Angstrom.t =
    not_char '/' >>| (fun c -> [c]) in
  let single_parser : string Angstrom.t =
    many (choice [ escaped_sep
                 ; not_sep
                 ])
    >>| List.concat
    >>| char_list_to_string
  in
  let path_parser   : string list Angstrom.t =
    sep_by sep single_parser in
  match Angstrom.parse_only path_parser (`String path) with
  | Ok lst  -> lst
  | Error _ -> assert false
;;

let path_to_file (path:string) : string =
  List.hd (List.rev (path_to_list path))
;;

let get_option_ref_init_if_none (eval:(unit -> 'a)) (opt_ref:'a option ref) : 'a =
  match !opt_ref with
  | Some x -> x
  | None   ->
    let x = eval () in
    opt_ref := Some x;
    x
;;

let pad_string (input:string) (len:int) (pad_char:char) : string =
  let input_len = String.length input in
  let pad_len   = len - input_len in
  let padding =
    if pad_len > 0 then
      String.make pad_len pad_char
    else
      "" in
  String.concat "" [input; padding]
;;

let round_down_to_multiple_int64 ~(multiple_of:int64) (x:int64) : int64 =
  let (</>) = Int64.div in
  let (<*>) = Int64.mul in
  (x </> multiple_of) <*> multiple_of
;;

let round_down_to_multiple ~(multiple_of:int) (x:int) : int =
  (x / multiple_of) * multiple_of
;;
