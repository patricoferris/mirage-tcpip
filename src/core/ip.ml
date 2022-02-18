type Error.t +=
  | No_route of string (** can't send a message to that destination *)
  | Would_fragment

let () = Error.register_printer ~id:"ip" ~title:"IP" ~pp:(function
  | No_route s -> Some Fmt.(const (fmt "no route to destination: %s") s)
  | Would_fragment -> Some Fmt.(const string "would fragment")
  | _ -> None)

type proto = [ `TCP | `UDP | `ICMP ]
let pp_proto ppf = function
  | `TCP -> Fmt.string ppf "TCP"
  | `UDP -> Fmt.string ppf "UDP"
  | `ICMP -> Fmt.string ppf "ICMP"

module type S = sig
  type ipaddr
  val pp_ipaddr : ipaddr Fmt.t
  type t
  val disconnect : t -> unit
  type callback = src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit
  val input:
    t ->
    tcp:callback -> udp:callback -> default:(proto:int -> callback) ->
    Cstruct.t -> unit
  val write: t -> ?fragment:bool -> ?ttl:int ->
    ?src:ipaddr -> ipaddr -> proto -> ?size:int -> (Cstruct.t -> int) ->
    Cstruct.t list -> unit Error.r
  val pseudoheader : t -> ?src:ipaddr -> ipaddr -> proto -> int -> Cstruct.t
  val src: t -> dst:ipaddr -> ipaddr
  val get_ip: t -> ipaddr list
  val mtu: t -> dst:ipaddr -> int
end
