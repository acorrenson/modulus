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
  | OpenL of int
  | OpenR of int
  | Intv of int * int

let top = Top

let bot = Bot

let is_empty x = x = Bot

let singleton v = Intv (v, v)

let to_string = function
  | Top -> "⊤"
  | Bot -> "⊥"
  | OpenL v -> Printf.sprintf "]-∞; %d]" v
  | OpenR v -> Printf.sprintf "[%d; +∞[" v
  | Intv (lo, hi) -> Printf.sprintf "[%d; %d]" lo hi

let pp_print fmt x = Format.fprintf fmt "%s" (to_string x)

let normalize = function
  | Intv (lo, hi) as i -> if lo > hi then Bot else i
  | _ as i -> i

let add x y =
  let r =
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
  | OpenL v -> OpenR (-v)
  | OpenR v -> OpenL (-v)
  | Intv (lo, hi) -> Intv (-hi, -lo)

let sub x y = add x (neg y)

let add_inv x y r = (sub r y, sub r x)

let _mid x y =
  let m = x / 2 + y / 2 in
  (m, m + (x land 1) * (y land 1))

let split = function
  | Top -> Split (OpenL 0, OpenR 0)
  | Bot -> failwith "cannot split ⊥"
  | Intv (lo, hi) ->
    if lo = hi then Single lo else
    let (m, m') = _mid lo hi in
    Split (Intv (lo, m), Intv (m', hi))
  | OpenL v ->
    Split (OpenL (v - 1), Intv (v, v))
  | OpenR v ->
    Split (Intv (v, v), OpenR (v + 1))

let peek = function
  | Top -> 0
  | Bot -> failwith "cannot peek a value in ⊥"
  | Intv (lo, _) -> lo
  | OpenL v | OpenR v -> v

end

module QfLia = struct
  module M = Solver.Make (Interval)
  let solve p =
    match M.solve p with
    | Model.UNKNOWN ->
      Last_effort.find_model p
    | _ as r -> r
end

module MiniLia = struct
open Monadsat.SolverMonad

type env = { n : int; doms : (Logic.term * Interval.t) list; vlist : string list; model : Model.t }

type res = Model.t

type solver = (env, res) t

let do_setv x d env = { env with doms = (x, d)::env.doms }, d
let do_update x d env = { env with doms = (x, d)::(List.remove_assoc x env.doms) }
let do_updatev x d env = do_update x d env, d

let update_dom x d =
  if Interval.is_empty d then 
    fail "unsat"
  else
    update (do_update x d)

let update_dom_ret x d = update_dom x d <&> return d

let rec eval (x : Logic.term) = step (fun env ->
  match List.assoc_opt x env.doms with
  | Some v -> return v
  | None ->
    match x with
    | Var _ ->
      update_dom_ret x Interval.top
    | Cst v -> 
      update_dom_ret x (Interval.singleton v)
    | Add (t1, t2) ->
      let* v1 = eval t1 in
      let* v2 = eval t2 in
      update_dom_ret x (Interval.add v1 v2)
)

let propagate_one (a : Logic.atom) =
  match a with
  | Logic.Eq (t1, t2) ->
    let* d1 = eval t1 in
    let* d2 = eval t2 in
    let d = Interval.inter d1 d2 in
    update_dom t1 d <&> update_dom t2 d
  | Logic.Ne (t1, t2) ->
    let* d1 = eval t1 in
    let* d2 = eval t2 in
    if d1 = d2 then
      update_dom t2 (Interval.inter d1 d2)
    else update (Fun.id)
  
let propagate (f : Logic.atom list) =
  List.fold_left (<&>) (update (Fun.id)) (List.map propagate_one f)

let decide v = update (fun env ->
  let x = List.hd env.vlist in
  let xs = List.tl env.vlist in
  let dx = Interval.singleton v in
  Printf.printf "decide %s := %d\n" x v;
  { env with doms = (Logic.Var x, dx)::List.remove_assoc (Logic.Var x) env.doms
  ; model = (x, v)::env.model
  ; vlist = xs
  }
)

let choice = function
  | [] -> assert false
  | x::xs -> List.fold_left (<|>) x xs

let decr = update (fun env -> { env with n = env.n - 1 })

let extract_model (f : Logic.atom list) = strategy (fun next {n; vlist; model; _} ->
  if n = 0 then abort
  else let next = decr <&> next in
  match vlist with
  | [] ->
    if Checker.check_list model f then return model
    else fail "extract model"
  | x::_ ->
    let* dx = eval (Var x) in
    match Interval.split dx with
    | Single v -> decide v <&> next
    | Split (d1, d2) ->
      let c1, c2 = Interval.peek d1, Interval.peek d2 in
      choice [
        decide c1 <&> next;
        decide c2 <&> next;
        update (do_update (Var x) d1) <&> propagate f <&> next;
        update (do_update (Var x) d2) <&> propagate f <&> next
      ])
let generic_solver (p : Logic.atom list) =
  propagate p <&> extract_model p

let solve p =
  let vlist = Logic.lvars p in
  let doms = [] in
  let n = 10 in
  let model = [] in
  run (generic_solver p) {n; doms; model; vlist}

end