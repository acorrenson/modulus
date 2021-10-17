(**
  {1 Lia}
  
  The Modulus internal solver for LIA

  Disclaimer : this module is a "work in progress"
*)

open Domain

module Interval : Domain = struct
type t =
  | Top
  | Bot
  | OpenL of Bigint.t
  | OpenR of Bigint.t
  | Intv of Bigint.t * Bigint.t

let top = Top

let bot = Bot

let is_empty x = x = Bot

let singleton v = Intv (v, v)

let to_string = function
  | Top -> "⊤"
  | Bot -> "⊥"
  | OpenL v -> Printf.sprintf "]-∞; %s]" (Bigint.to_string v)
  | OpenR v -> Printf.sprintf "[%s; +∞[" (Bigint.to_string v)
  | Intv (lo, hi) -> Printf.sprintf "[%s; %s]" (Bigint.to_string lo) (Bigint.to_string hi)

let pp_print fmt x = Format.fprintf fmt "%s" (to_string x)

let normalize = function
  | Intv (lo, hi) as i -> if lo > hi then Bot else i
  | _ as i -> i

let add x y =
  let r = let open Num.Make(Bigint) in 
    match x, y with
    | Bot, _ | _, Bot -> Bot
    | Top, _ | _, Top -> Top
    | OpenL v, OpenL v' -> OpenL (v + v')
    | OpenR v, OpenR v' -> OpenR (v + v')
    | OpenL _, OpenR _ | OpenR _, OpenL _ -> Top
    | Intv (_, hi), OpenL v | OpenL v, Intv (_, hi) -> OpenL (hi + v)
    | Intv (lo, _), OpenR v | OpenR v, Intv (lo, _) -> OpenR (lo + v)
    | Intv (lo, hi), Intv (lo', hi') -> Intv (lo + lo', hi + hi')
  in normalize r

let inter x y =
  let r =
    match x, y with
    | Bot, _ | _, Bot -> Bot
    | Top, x | x, Top -> x
    | OpenL v, OpenL v' -> OpenL (min v v')
    | OpenR v, OpenR v' -> OpenR (max v v')
    | OpenL v, OpenR v' | OpenR v', OpenL v -> Intv (min v v', max v v')
    | Intv (lo, hi), Intv (lo', hi') -> Intv (max lo lo', min hi hi')
    | OpenL v, Intv (lo, _) | Intv (lo, _), OpenL v -> OpenL (min v lo)
    | OpenR v, Intv (_, hi) | Intv (_, hi), OpenR v -> OpenR (max v hi)
  in normalize r

let neg = function
  | Bot -> Bot
  | Top -> Top
  | OpenL v -> OpenR (Bigint.neg v)
  | OpenR v -> OpenL (Bigint.neg v)
  | Intv (lo, hi) -> Intv (Bigint.neg hi, Bigint.neg lo)

let sub x y = add x (neg y)

let add_inv x y r = (sub r y, sub r x)

let split = let open Num.MakeInt(Bigint) in function
  | Top -> Split (OpenL Bigint.zero, OpenR Bigint.zero)
  | Bot -> failwith "cannot split ⊥"
  | Intv (lo, hi) ->
    if lo = hi then Single lo else
    let (m, m') = mid lo hi in
    Split (Intv (lo, m), Intv (m', hi))
  | OpenL v ->
    Split (OpenL (v - one), Intv (v, v))
  | OpenR v ->
    Split (Intv (v, v), OpenR (v + one))

let peek = function
  | Top -> Bigint.zero
  | Bot -> failwith "cannot peek a value in ⊥"
  | Intv (lo, _) -> lo
  | OpenL v | OpenR v -> v

end

module QfLia = Solver.Make (Interval)
