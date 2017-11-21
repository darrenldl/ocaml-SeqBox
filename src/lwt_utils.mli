module IO : sig
  val read_with_jitter : buf:bytes -> Lwt_io.input_channel -> len:int -> int Lwt.t
end
