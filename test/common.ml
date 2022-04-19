module Lwt = struct end

let failf fmt = Fmt.kstr (fun s -> Alcotest.fail s) fmt
let ( let* ) = Result.bind

let or_error name fn t =
  match fn t with Error _ -> failf "or_error starting %s" name | Ok t -> t

let expect_error error name fn t =
  match fn t with
  | Error error2 when Error.head error2 = error -> t
  | _ -> failf "expected error on %s" name

let ipv4_packet = Alcotest.testable Ipv4_packet.pp Ipv4_packet.equal
let udp_packet = Alcotest.testable Udp_packet.pp Udp_packet.equal
let tcp_packet = Alcotest.testable Tcp.Tcp_packet.pp Tcp.Tcp_packet.equal
let cstruct = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal

let sequence =
  let eq x y = Tcp.Sequence.compare x y = 0 in
  Alcotest.testable Tcp.Sequence.pp eq

let options = Alcotest.testable Tcp.Options.pp Tcp.Options.equal

let run_dir program () =
  Eio_linux.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let dir = Eio.Stdenv.fs env in
  try
    Eio.Switch.run @@ fun sw ->
    program ~sw ~clock ~dir ();
    Eio.Switch.fail sw Not_found
  with Not_found -> ()

let run program =
  run_dir (fun ~sw ~clock ~dir:_ () -> program ~sw ~clock ())
