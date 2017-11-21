type 'a t

val create        : ?overwrite:bool -> init_val:'a -> int -> 'a t

val put           : 'a t -> 'a -> unit Lwt.t

val take          : 'a t -> 'a Lwt.t

val put_no_block  : 'a t -> 'a -> bool Lwt.t

val take_no_block : 'a t -> 'a option Lwt.t

val clear         : ?dummy_val:'a -> 'a t -> unit Lwt.t

val enable        : 'a t -> unit Lwt.t

val disable       : ?dummy_val:'a -> 'a t -> unit Lwt.t

val is_enabled    : 'a t -> bool Lwt.t

val is_disabled   : 'a t -> bool Lwt.t
