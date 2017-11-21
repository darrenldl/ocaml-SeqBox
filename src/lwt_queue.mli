type 'a t

val create : ?overwrite:bool -> init_val:'a -> int -> 'a t

val put    : 'a t -> 'a -> unit Lwt.t

val take   : 'a t -> 'a Lwt.t

val put_no_block : 'a t -> 'a -> bool Lwt.t

val take_no_block : 'a t -> 'a option Lwt.t
