let protect ~(f:unit -> 'a) ~(finally:unit -> unit) : 'a =
  let finally_executed : bool ref = ref false in
  let res : 'a =
    try
      f ()
    with
    | e -> finally_executed := true; finally (); raise e in
  if !finally_executed then
    res
  else
    (finally (); res)
;;

let lwt_protect ~(f:unit -> 'a Lwt.t) ~(finally:unit -> unit Lwt.t) : 'a Lwt.t =
  let finally_executed : bool ref = ref false in
  let%lwt res =
    try%lwt
      f ()
    with
    | e ->
      finally_executed := true;
      finally () >>
      raise e in
  if !finally_executed then
    Lwt.return res
  else
    (finally () >> Lwt.return res)
;;
