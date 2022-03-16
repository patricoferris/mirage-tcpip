(*
 * Copyright (c) 2012 Balraj Singh <bs375@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Lwt = struct end

let src = Logs.Src.create "tcptimer" ~doc:"Mirage TCP Tcptimer module"
module Log = (val Logs.src_log src : Logs.LOG)

type time = int64

type tr =
  | Stoptimer
  | Continue of Sequence.t
  | ContinueSetPeriod of (time * Sequence.t)

type t = {
  expire: (Sequence.t -> tr);
  mutable period_ns: time;
  mutable running: bool;
  clock: Eio.Time.clock;
}

let t ~period_ns ~expire ~clock =
  let running = false in
  {period_ns; expire; running; clock}

let timerloop t s =
  Log.debug (fun f -> f "timerloop");
  Stats.incr_timer ();
  let rec aux t s =
    Log.debug (fun f -> f "timerloop: sleeping for %Lu ns" t.period_ns);
    Eio.Time.sleep t.clock (Int64.to_float t.period_ns /. 1_000_000_000.);
    match t.expire s with
    | Stoptimer ->
      Stats.decr_timer ();
      t.running <- false;
      Log.debug (fun f -> f "timerloop: stoptimer");
      ()
    | Continue d ->
      Log.debug (fun f -> f "timerloop: continuer");
      aux t d
    | ContinueSetPeriod (p, d) ->
      Log.debug (fun f -> f "timerloop: continuesetperiod (new period: %Lu ns)" p);
      t.period_ns <- p;
      aux t d
  in
  aux t s

let period_ns t = t.period_ns

let start ~sw t ?(p=(period_ns t)) sequence =
  if not t.running then begin
    t.period_ns <- p;
    t.running <- true;
    Eio.Fiber.fork ~sw (fun () -> timerloop t sequence);
    ()
  end else
    ()