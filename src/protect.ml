let protect ~(f:unit -> 'a) ~(finally:unit -> unit) : 'a =
  let finally_executed : bool ref = ref false in
  let res : 'a =
    try
      f ()
    with
    | exn -> finally_executed := true; finally (); raise exn in
  if !finally_executed then
    res
  else
    (finally (); res)
;;
