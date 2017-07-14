open Stdint

exception File_access_error

let getmtime ~(filename:string) : float =
  (* Unix.stat should also be usable on windows
   * Reference :
   *  https://caml.inria.fr/pub/docs/manual-ocaml/libunix.html (Accessed on 2017-06-29)
   *    See table shown at bottom, Unix.stat is not in the table
   *)
  try
    let { Unix.st_mtime = mtime; _ } = Unix.stat filename in
    mtime
  with
  | _ -> raise File_access_error
;;

let getmtime_uint64 ~(filename:string) : uint64 =
  Uint64.of_float (getmtime ~filename)
;;

let getsize ~(filename:string) : int64 =
  try
    let { Unix.LargeFile.st_size = size; _ } = Unix.LargeFile.stat filename in
    size
  with
  | _ -> raise File_access_error
;;

let getsize_uint64 ~(filename:string) : uint64 =
    Uint64.of_int64 (getsize ~filename)
;;
