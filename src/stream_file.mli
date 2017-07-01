module Stream : sig
  type 'a in_out_processor = Core.In_channel.t -> Core.Out_channel.t -> ('a, string) result

  val process_in_out :
    in_filename:string -> out_filename:string -> processor:('a in_out_processor)
    -> ('a, string) result
end

module Helper : sig
  val read_chunk_into_buf : ?offset:int       -> ?len:int -> Core.In_channel.t  -> buf:bytes -> bool * int

  val read_chunk          : Core.In_channel.t -> len:int  -> bool * bytes

  val write_from_buf      : ?offset:int       -> ?len:int -> Core.Out_channel.t -> buf:bytes -> unit
end
