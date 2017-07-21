let protect ~(f:unit -> 'a) ~(finally:unit -> unit) : 'a =
  try
    let res = f () in
    finally ();
    res
  with
  | exn -> finally (); raise exn
;;
