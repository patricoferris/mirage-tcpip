(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS l SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "ipv4" ~doc:"Mirage IPv4"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (R: Mirage_random.S) (C: Mirage_clock.MCLOCK) (Ethernet: Ethernet.S) (Arpv4 : Arp.S) = struct
  module Routes = Routing.Make(Log)(Arpv4)

  type ipaddr = Ipaddr.V4.t
  type callback = src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit

  let pp_ipaddr = Ipaddr.V4.pp

  type t = {
    ethif : Ethernet.t;
    arp : Arpv4.t;
    cidr: Ipaddr.V4.Prefix.t;
    gateway: Ipaddr.V4.t option;
    mutable cache: Fragments.Cache.t;
  }

  let write t ?(fragment = true) ?(ttl = 38) ?src dst proto ?(size = 0) headerf bufs =
    match Routes.destination_mac t.cidr t.gateway t.arp dst with
    | Error e when Error.head e = Routing.Local ->
      Log.warn (fun f -> f "Could not find %a on the local network" Ipaddr.V4.pp dst);
      Error.add_error ~__POS__ (Tcpip.Ip.No_route "no response for IP on local network") (Error e)
    | Error e when t.gateway = None && Error.head e = Routing.Gateway ->
      Log.warn (fun f -> f "Write to %a would require an external route, which was not provided" Ipaddr.V4.pp dst);
      Ok ()
    | Error e when Error.head e = Routing.Local ->
      Log.warn (fun f -> f "Write to %a requires an external route, and the provided %a was not reachable" Ipaddr.V4.pp dst (Fmt.option Ipaddr.V4.pp) t.gateway);
      (* when a gateway is specified the user likely expects their traffic to be passed to it *)
      Error.add_error ~__POS__ (Tcpip.Ip.No_route "no route to default gateway to outside world") (Error e)
    | Error e -> Error e
    | Ok mac ->
      (* need first to deal with fragmentation decision - find out mtu *)
      let mtu = Ethernet.mtu t.ethif in
      (* no options here, always 20 bytes! *)
      let hdr_len = Ipv4_wire.sizeof_ipv4 in
      let needed_bytes = Cstruct.lenv bufs + hdr_len + size in
      let multiple = needed_bytes > mtu in
      (* construct the header (will be reused across fragments) *)
      if not fragment && multiple then
        (Error.v ~__POS__ Tcpip.Ip.Would_fragment)
      else
        let off =
          match fragment, multiple with
          | true, true -> 0x2000
          | false, false -> 0x4000
          | true, false -> 0x0000
          | false, true -> assert false (* handled by conditional above *)
        in
        let hdr =
          let src = match src with None -> Ipaddr.V4.Prefix.address t.cidr | Some x -> x in
          let id = if multiple then Randomconv.int16 R.generate else 0 in
          Ipv4_packet.{
            options = Cstruct.empty ;
            src ; dst ; ttl ; off ; id ;
            proto = Ipv4_packet.Marshal.protocol_to_int proto }
        in
        let writeout size fill =
          match Ethernet.write t.ethif mac `IPv4 ~size fill with
          | Error e ->
            Log.warn (fun f -> f "Error sending Ethernet frame: %a"
                         Error.pp (Error.head e));
            Error e
          | Ok () -> Ok ()
        in
        Log.debug (fun m -> m "ip write: mtu is %d, hdr_len is %d, size %d \
                               payload len %d, needed_bytes %d"
                      mtu hdr_len size (Cstruct.lenv bufs) needed_bytes) ;
        let leftover = ref Cstruct.empty in
        (* first fragment *)
        let fill buf =
          let payload_buf = Cstruct.shift buf hdr_len in
          let header_len = headerf payload_buf in
          if header_len > size then begin
            Log.err (fun m -> m "headers returned length exceeding size") ;
            invalid_arg "headerf exceeds size"
          end ;
          (* need to copy the given payload *)
          let len, rest =
            Cstruct.fillv ~src:bufs ~dst:(Cstruct.shift payload_buf header_len)
          in
          leftover := Cstruct.concat rest;
          let payload_len = header_len + len in
          match Ipv4_packet.Marshal.into_cstruct ~payload_len hdr buf with
          | Ok () -> payload_len + hdr_len
          | Error msg ->
            Log.err (fun m -> m "failure while assembling ip frame: %s" msg) ;
            invalid_arg msg
        in
        match writeout (min mtu needed_bytes) fill with
        | Error e -> Error e
        | Ok () ->
          if not multiple then
            Ok ()
          else
            let remaining = Fragments.fragment ~mtu hdr !leftover in
            List.fold_left (fun acc p ->
                match acc with
                | Error e -> Error e
                | Ok () ->
                  let l = Cstruct.length p in
                  writeout l (fun buf -> Cstruct.blit p 0 buf 0 l ; l))
              (Ok ()) remaining

  let input t ~tcp ~udp ~default buf =
    match Ipv4_packet.Unmarshal.of_cstruct buf with
    | Error s ->
      Log.info (fun m -> m "error %s while parsing IPv4 frame %a" s Cstruct.hexdump_pp buf)
    | Ok (packet, payload) ->
      let of_interest ip =
        Ipaddr.V4.(compare ip (Prefix.address t.cidr) = 0
                   || compare ip broadcast = 0
                   || compare ip (Prefix.broadcast t.cidr) = 0)
      in
      if not (of_interest packet.dst) then begin
        Log.debug (fun m -> m "dropping IP fragment not for us or broadcast %a"
                      Ipv4_packet.pp packet)
      end else if Cstruct.length payload = 0 then begin
        Log.debug (fun m -> m "dropping zero length IPv4 frame %a" Ipv4_packet.pp packet)
      end else
        let ts = C.elapsed_ns () in
        let cache, res = Fragments.process t.cache ts packet payload in
        t.cache <- cache ;
        match res with
        | None -> ()
        | Some (packet, payload) ->
          let src, dst = packet.src, packet.dst in
          match Ipv4_packet.Unmarshal.int_to_protocol packet.proto with
          | Some `TCP -> tcp ~src ~dst payload
          | Some `UDP -> udp ~src ~dst payload
          | Some `ICMP | None -> default ~proto:packet.proto ~src ~dst payload

  let connect
      ?(no_init = false) ~cidr ?gateway ?(fragment_cache_size = 1024 * 256) ethif arp =
    (if no_init then
       ()
     else
       Arpv4.set_ips arp [Ipaddr.V4.Prefix.address cidr]);
    let cache = Fragments.Cache.empty fragment_cache_size in
    { ethif; arp; cidr; gateway; cache }

  let disconnect _ = ()

  let get_ip t = [Ipaddr.V4.Prefix.address t.cidr]

  let pseudoheader t ?src dst proto len =
    let src = match src with None -> Ipaddr.V4.Prefix.address t.cidr | Some x -> x in
    Ipv4_packet.Marshal.pseudoheader ~src ~dst ~proto len

  let src t ~dst:_ = Ipaddr.V4.Prefix.address t.cidr

  let mtu t ~dst:_ = Ethernet.mtu t.ethif - Ipv4_wire.sizeof_ipv4

end
