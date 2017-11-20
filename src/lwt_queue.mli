type 'a t

val create : ?overwrite:bool -> init_val:'a -> int -> 'a t

val put    : 'a t -> 'a -> unit Lwt.t

val take   : 'a t -> 'a Lwt.t
