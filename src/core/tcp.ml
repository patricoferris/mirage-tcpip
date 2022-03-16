module Keepalive = struct
  type t = {
    after: Duration.t;
    interval: Duration.t;
    probes: int;
  }
end

module type S = sig
  type ipaddr
  type t
  val disconnect : t -> unit

  val create_connection: ?keepalive:Keepalive.t -> t -> ipaddr * int -> <Eio.Flow.two_way; Eio.Flow.close> Error.r
  val listen : t -> port:int -> ?keepalive:Keepalive.t -> (<Eio.Flow.two_way; Eio.Flow.close> -> unit) -> unit
  val unlisten : t -> port:int -> unit

  val input: t -> src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit
end
