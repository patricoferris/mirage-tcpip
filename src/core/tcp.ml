module Keepalive = struct
  type t = {
    after: Duration.t;
    interval: Duration.t;
    probes: int;
  }
end

module type S = sig
  type ipaddr
  type flow
  type t
  val disconnect : t -> unit
  include Mirage_flow.S with
      type flow   := flow

  val dst: flow -> ipaddr * int
  val write_nodelay: flow -> Cstruct.t -> unit Error.r
  val writev_nodelay: flow -> Cstruct.t list -> unit Error.r
  val create_connection: ?keepalive:Keepalive.t -> t -> ipaddr * int -> flow Error.r
  val listen : t -> port:int -> ?keepalive:Keepalive.t -> (flow -> unit) -> unit
  val unlisten : t -> port:int -> unit
  val input: t -> src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit
end
