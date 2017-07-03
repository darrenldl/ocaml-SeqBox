module Stream : sig
  type 'a in_out_processor = Core.In_channel.t  -> Core.Out_channel.t -> 'a

  type 'a in_processor     = Core.In_channel.t  -> 'a

  type 'a out_processor    = Core.Out_channel.t -> 'a

  val process_in_out :
    in_filename:string -> out_filename:string -> processor:('a in_out_processor)
    -> ('a, string) result

  val process_in  :
    in_filename:string  -> processor:('a in_processor)
    -> ('a, string) result

  val process_out :
    out_filename:string -> processor:('a out_processor)
    -> ('a, string) result
end

(* General helpers *)
module General_helper : sig
  val make_buffer : int -> bytes
end

(* Helpers for reading into buffer *)
module Read_into_buf : sig
  exception Invalid_offset
  exception Invalid_length

  type read_result = { no_more_bytes : bool
                     ; read_count    : int
                     }

  val read : ?offset:int -> ?len:int -> Core.In_channel.t -> buf:bytes -> read_result
end

(* Helpers for reading and returning data as value *)
module Read_chunk : sig
  type read_result = { no_more_bytes : bool
                     ; chunk         : bytes
                     }

  val read : Core.In_channel.t -> len:int -> read_result
end

(* Helpers for writing from buffer *)
module Write_from_buf : sig
  exception Invalid_offset
  exception Invalid_length

  val write : ?offset:int -> ?len:int -> Core.Out_channel.t -> buf:bytes -> unit
end

module Write_chunk : sig
  val write : Core.Out_channel.t -> buf:bytes -> unit
end
