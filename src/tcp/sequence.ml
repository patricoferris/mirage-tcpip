(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
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

(* TCP sequence numbers must work with overflow, so this puts them in a
   separate type to make sure they dont get mixed up *)


external ( = ) : int -> int -> bool = "%equal"
external ( <> ) : int -> int -> bool = "%notequal"
external ( < ) : int -> int -> bool = "%lessthan"
external ( > ) : int -> int -> bool = "%greaterthan"
external ( <= ) : int -> int -> bool = "%lessequal"
external ( >= ) : int -> int -> bool = "%greaterequal"
external compare : int -> int -> int = "%compare"
external equal : int -> int -> bool = "%equal"

type t = int

(* a < b *)
let lt a b = a < b

(* a <= b *)
let leq a b = a <= b

(* a > b *)
let gt a b = a > b

(* a >= b *)
let geq a b = a >= b

(* b <= a <= c *)
let between a b c = (geq a b) && (leq a c)

(* a + b *)
let add a b = a + b

(* a - b *)
let sub a b = a - b

(* a + 1 *)
let succ a = a + 1

(* a - 1 *)
let pred a = a - 1

let compare a b = compare a b
let of_int32 t = Int32.to_int t
let of_int t = t
let to_int32 t = Int32.of_int t
let to_int t = t

let zero = 0

let pp fmt t = Fmt.int fmt t
    