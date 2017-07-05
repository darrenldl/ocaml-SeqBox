exception Packaged_exn of string

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
  exception Invalid_range

  val make_buffer            : int       -> bytes

  val get_from_buf           : buf:bytes -> pos:int      -> len:int        -> bytes

  (* Inclusive range *)
  val get_from_buf_inc_range : buf:bytes -> start_at:int -> end_at:int     -> bytes

  (* Exclusive range *)
  val get_from_buf_exc_range : buf:bytes -> start_at:int -> end_before:int -> bytes
end

(* Helpers for reading into buffer *)
module Read_into_buf : sig
  exception Invalid_offset
  exception Invalid_length

  type read_stats  = { read_count : int }
  type read_result = read_stats option

  val read : ?offset:int -> ?len:int -> Core.In_channel.t -> buf:bytes -> read_result
end

(* Helpers for reading and returning data as value *)
module Read_chunk : sig
  type read_content = { chunk : bytes }
  type read_result  = read_content option

  val read : Core.In_channel.t -> len:int -> read_result
end

(* Helpers for writing from buffer *)
module Write_from_buf : sig
  exception Invalid_offset
  exception Invalid_length

  val write : ?offset:int -> ?len:int -> Core.Out_channel.t -> buf:bytes -> unit
end

module Write_chunk : sig
  val write : Core.Out_channel.t -> chunk:bytes -> unit
end
