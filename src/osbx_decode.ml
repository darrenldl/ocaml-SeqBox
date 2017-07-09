open Cmdliner
open Decode

exception Packaged_exn of string

let decode (force_out:bool) (in_filename:string) (out_filename:string option) : unit =
  try
    let out_filename : string option =
      match Process.fetch_out_filename ~in_filename ~out_filename with
      | Ok name   -> name
      | Error msg -> raise (Packaged_exn msg) in
    match out_filename with
    | None              -> raise (Packaged_exn "No original filename found in sbx container and no output file name is provided")
    | Some out_filename ->
      let out_file_exists = Sys.file_exists out_filename in
      if out_file_exists && not force_out then
        raise (Packaged_exn (Printf.sprintf "File %s already exists" out_filename))
      else
        match Process.decode_file ~in_filename ~out_filename:(Some out_filename) with
        | Ok stats  -> Stats.print_stats stats
        | Error msg -> raise (Packaged_exn msg)
  with
  | Packaged_exn msg -> Printf.printf "%s\n" msg

let in_file =
  let doc = "Sbx container to decode" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INFILE" ~doc)
;;

let out_file =
  let doc = "Decoded file name (defaults to name stored in sbx container if present)" in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"OUTFILE" ~doc)
;;
