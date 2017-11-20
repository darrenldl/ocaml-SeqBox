type 'a t

val create : init_val:'a -> size:int -> 'a t

val put    : 'a t -> 'a -> unit Lwt.t

val take   : 'a t -> 'a Lwt.t
