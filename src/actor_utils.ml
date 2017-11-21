let bind_to_queue ~(convert:'a -> 'b) (actor : 'a Lwt.t) (result_queue : 'b Lwt_queue.t)
  : unit Lwt.t =
  Lwt.bind actor (fun res -> Lwt_queue.put result_queue (convert res))
;;

let monitor
    ~(count:int)
    ~(fail_pred : 'a -> bool)
    ~(monitor_queue:'a Lwt_queue.t)
  : 'a list Lwt.t =
  let rec go (count:int) (acc:'a list) : 'a list Lwt.t =
    if count <= 0 then Lwt.return acc
    else
      let%lwt res = Lwt_queue.take monitor_queue in
      if fail_pred res then Lwt.return acc
      else go (count - 1) (res :: acc) in
  go count []
;;

