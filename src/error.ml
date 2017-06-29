type error =
    Bytes_too_long      of (Sbx_block.Metadata.id * int) list
  | Failed_to_read_file of string

let bytes_too_long_to_string (lst:Sbx_block.Metadata.id * int) : string =
  let open Sbx_block.Metadata in
  let bytes_too_long_to_string_helper (lst:id * int) (acc:string list) : string =
    match lst with
    | []              -> (String.concat "" (List.rev acc))
    | (id, len) :: vs -> let str = Printf.sprintf "id : %s, len : %d\n" (id_to_string id) len in
      bytes_too_long_to_string_helper vs (str :: acc) in
  let distribution_str = bytes_too_long_to_string_helper lst [] in
  String.concat "" ["the length of the metadata:\n"; distribution_str]
;;

let error_to_string (err:error) : string =
  match err with
  | Bytes_too_long      v -> bytes_too_long_to_string v
  | Failed_to_read_file v -> Printf.sprintf "failed to read file : %s" v
;;
