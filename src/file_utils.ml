open Stdint

let getmtime ~(filename:string) : (float, string) result =
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
