type write_req =
  | No_location of string
  | With_location of int64 * string

val gen_file_reader :
  filename:string ->
  chunk_size:int ->
  out_queue:string option Lwt_queue.t ->
  (unit -> (unit, string) result Lwt.t)

val gen_file_writer :
  filename:string ->
  in_queue:(write_req option Lwt_queue.t) ->
  (unit -> (unit, string) result Lwt.t)

val gen_duplicator :
  in_queue:('a Lwt_queue.t) ->
  out_queues:('a Lwt_queue.t list) ->
  stop_pred:('a -> bool) ->
  (unit -> unit Lwt.t)
