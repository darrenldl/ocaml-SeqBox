open Core

module Stream : sig
  val process_in_out :
    in_filename:string -> out_filename:string -> processor:(In_channel.t -> Out_channel.t -> ('a, string) result)
    -> ('a, string) result
end
