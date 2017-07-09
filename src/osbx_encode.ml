open Cmdliner
open Encode

exception Packaged_exn of string

let encode (force_out:bool) (no_meta:bool) (uid:string option) (in_filename:string) (out_filename:string option) : unit =
  try
    let uid : bytes option =
      match uid with
      | None     -> None
      | Some str ->
        match Conv_utils.hex_string_to_bytes str with
        | Ok uid  -> Some uid
        | Error _ -> raise (Packaged_exn (Printf.sprintf "Uid %s is not a valid hex string" str)) in
    let out_filename : string =
      match out_filename with
      | None     -> Bytes.concat "" [in_filename; ".sbx"]
      | Some str -> str in
    let out_file_exists = Sys.file_exists out_filename in
    if out_file_exists && not force_out then
      raise (Packaged_exn (Printf.sprintf "File %s already exists" out_filename))
    else
      match Process.encode_file ~uid ~want_meta:(not no_meta) ~in_filename ~out_filename with
      | Ok stats  -> Stats.print_stats stats
      | Error msg -> raise (Packaged_exn (Printf.sprintf "Error : %s" msg))
  with 
  | Packaged_exn str -> Printf.printf "%s\n" str
;;

let uid =
  let doc = "Alternative file uid (by default uid is randomly generated)" in
  Arg.(value & opt (some string) None & info ["uid"] ~doc)
;;

let no_meta =
  let doc = "No metadata block" in
  Arg.(value & flag & info ["nometa"] ~doc)
;;

let in_file =
  let doc = "File to encode" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INFILE" ~doc)
;;

let out_file =
  let doc = "Sbx container name (defaults to INFILE.sbx)" in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"OUTFILE" ~doc)
;;
