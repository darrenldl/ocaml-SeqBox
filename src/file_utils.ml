open Stdint

let getmtime ~(filename:string) : (float, string) result =
  (* Unix.stat should also be usable on windows
   * Reference :
   *  https://caml.inria.fr/pub/docs/manual-ocaml/libunix.html (Accessed on 2017-06-29)
   *    See table shown at bottom, Unix.stat is not in the table
   *)
  try
    let { Unix.st_mtime = mtime; _ } = Unix.stat filename in
    Ok mtime
  with
  | _ -> Error (Printf.sprintf "failed to get stat of file : %s" filename)
;;

let getmtime_uint64 ~(filename:string) : (uint64, string) result =
  match getmtime ~filename with
  | Ok    mtime -> Ok    (Uint64.of_float mtime)
  | Error msg   -> Error msg
;;
