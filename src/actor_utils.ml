let bind_to_queue (actor : 'a Lwt.t) (result_queue : 'a Lwt_queue.t)
  : unit Lwt.t =
  Lwt.bind actor (fun res -> Lwt_queue.put result_queue res)
;;
