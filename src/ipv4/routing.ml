(* RFC 1112: 01-00-5E-00-00-00 ORed with lower 23 bits of the ip address *)
let mac_of_multicast ip =
  let ipb = Ipaddr.V4.to_octets ip in
  let macb = Bytes.create 6 in
  Bytes.set macb 0 (Char.chr 0x01);
  Bytes.set macb 1 (Char.chr 0x00);
  Bytes.set macb 2 (Char.chr 0x5E);
  Bytes.set macb 3 (Char.chr ((Char.code ipb.[1]) land 0x7F));
  Bytes.set macb 4 (String.get ipb 2);
  Bytes.set macb 5 (String.get ipb 3);
  Macaddr.of_octets_exn (Bytes.to_string macb)

type Error.t += Local | Gateway

let () = Error.register_printer ~id:"ip.routing" ~title:"IP routing" ~pp:(function
  | Local -> Some Fmt.(const string "Local routing failed")
  | Gateway -> Some Fmt.(const string "Gateway routing failed")
  | _ -> None
)

module Make(Log : Logs.LOG) (A : Arp.S) = struct

  let destination_mac network gateway arp = function
    |ip when Ipaddr.V4.(compare ip broadcast) = 0
          || Ipaddr.V4.(compare ip any) = 0
          || Ipaddr.V4.(compare (Prefix.broadcast network) ip) = 0 -> (* Broadcast *)
      Ok Macaddr.broadcast
    |ip when Ipaddr.V4.is_multicast ip ->
      Ok (mac_of_multicast ip)
    |ip when Ipaddr.V4.Prefix.mem ip network -> (* Local *)
      begin
      match A.query arp ip with
      | Ok mac -> Ok mac
      | Error e when Error.head e = A.Timeout ->
        Log.info (fun f ->
            f "IP.output: could not determine link-layer address for local \
                network (%a) ip %a" Ipaddr.V4.Prefix.pp network
              Ipaddr.V4.pp ip);
        Error.add_error ~__POS__ Local (Error e)
      | Error e ->
        Log.info (fun f -> f "IP.output: %a" Error.pp_trace e);
        Error.add_error ~__POS__ Local (Error e)
      end
    |ip -> (* Gateway *)
      match gateway with
      | None ->
        Log.info (fun f ->
            f "IP.output: no route to %a (no default gateway is configured)"
              Ipaddr.V4.pp ip);
        Error.v ~__POS__ Gateway
      | Some gateway ->
        match A.query arp gateway with
        | Ok mac -> Ok mac
        | Error e when Error.head e = A.Timeout ->
          Log.info (fun f ->
              f "IP.output: could not send to %a: failed to contact gateway %a"
                Ipaddr.V4.pp ip Ipaddr.V4.pp gateway);
          Error.add_error ~__POS__ Gateway (Error e)
        | Error e ->
          Log.info (fun f -> f "IP.output: %a" Error.pp_trace e);
          Error.add_error ~__POS__ Gateway (Error e)
end
