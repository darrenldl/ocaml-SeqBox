module Writer : sig
  type write_req =
    | No_position   of string
    | With_position of int64 * string
    | Set_position  of int64
    | Get_position

  type reply =
    | Position of int64
end

val gen_file_reader :
  filename:string ->
  chunk_size:int ->
  out_queue:string option Lwt_queue.t ->
  (unit -> (unit, string) result Lwt.t)

val gen_file_writer :
  filename:string ->
  in_queue:Writer.write_req option Lwt_queue.t ->
  reply_queue:Writer.reply Lwt_queue.t ->
  (unit -> (unit, string) result Lwt.t)

val gen_duplicator :
  in_queue:('a Lwt_queue.t) ->
  out_queues:('a Lwt_queue.t list) ->
  stop_pred:('a -> bool) ->
  (unit -> unit Lwt.t)
