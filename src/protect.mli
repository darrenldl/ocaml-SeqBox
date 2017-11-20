val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a

val lwt_protect : f:(unit -> 'a Lwt.t) -> finally:(unit -> unit Lwt.t) -> 'a Lwt.t
