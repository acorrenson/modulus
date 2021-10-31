(**
  {1 Lia}
  
  The Modulus internal solver for LIA

  Disclaimer : this module is a "work in progress"
*)

open Domain
open Strategy

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



let asp = Format.asprintf

module DomMap = struct
type t = { n : int; doms : (Logic.term * Interval.t) list; vlist : string list; model : Model.t }

let rec pp_print fmt = function
  | [] -> ()
  | (x, d)::next ->
    Format.fprintf fmt "%a := %a, %a"
      Logic.pp_term x
      Interval.pp_print d
      pp_print next

let set_dom_aux x d env =
  { env with doms = (x, d)::env.doms }

let set_dom x d = update (set_dom_aux x d)

let set_ret_dom x d = update_ret d (set_dom_aux x d)

let rec eval (x : Logic.term) = step @@ fun env ->
  (* asp "evaluating %a in ctx [%a...]" Logic.pp_term x pp_print env.doms <?> *)
  match List.assoc_opt x env.doms with
  | Some v ->
    (* asp "find %a := %a" Logic.pp_term x Interval.pp_print v <?> *)
    return v
  | None ->
    match x with
    | Var _ ->
      (* asp "set %a := %a" Logic.pp_term x Interval.pp_print (Interval.top) <?> *)
      set_ret_dom x Interval.top
    | Cst v ->
      (* asp "set %a := %a" Logic.pp_term x Interval.pp_print (Interval.singleton v) <?> *)
      set_ret_dom x (Interval.singleton v)
    | Add (t1, t2) ->
      let* v1 = eval t1 in
      let* v2 = eval t2 in
      (* asp "set %a := %a" Logic.pp_term x Interval.pp_print (Interval.add v1 v2) <?> *)
      set_ret_dom x (Interval.add v1 v2)

let update_dom x d =
  (* asp "update %a := %a" Logic.pp_term x Interval.pp_print d <?> *)
  let* dx = eval x in
  let dx' = Interval.inter dx d in
  if Interval.is_empty dx' then
    fail "unsat"
  else update @@ fun env ->
    { env with doms = (x, d)::(List.remove_assoc x env.doms) }

let update_ret_dom x d = update_dom x d <&> return d
end

module MiniLia = struct

type solver = (DomMap.t, Model.t) t
open DomMap

let no_update = update (Fun.id)

let debug = step @@ fun env ->
  asp "In ctx [%a...]" pp_print env.doms <?> no_update

let rec propagate_one_back t d =
  let open Logic in
  (* asp "propagating backward %a := %a" Logic.pp_term t Interval.pp_print d <?> *)
  match t with
  | Cst _ | Var _ -> update_dom t d
  | Add (t1, t2) ->
    let* d1 = eval t1 in
    let* d2 = eval t2 in
    let (d1', d2') = Interval.add_inv d1 d2 d in
    propagate_one_back t1 d1' <&> propagate_one_back t2 d2'

let propagate_one (a : Logic.atom) =
  (* asp "propagating %a" Logic.pp_atom a <?> *)
  match a with
  | Logic.Eq (t1, t2) ->
    let* d1 = eval t1 in
    let* d2 = eval t2 in
    let d = Interval.inter d1 d2 in
    propagate_one_back t1 d <&> propagate_one_back t2 d
  | Logic.Ne (t1, t2) ->
    let* d1 = eval t1 in
    let* d2 = eval t2 in
    if d1 = d2 then fail "unsat"
    else no_update

let propagate (f : Logic.atom list) =
  (* asp "propagate" <?> *)
  List.fold_left (<&>) no_update (List.map propagate_one f)

let decide v = step (fun env ->
  let x = List.hd env.vlist in
  let xs = List.tl env.vlist in
  let dx = Interval.singleton v in
  (* asp "decide %s := %d" x v <?> *)
  set { env with doms = (Logic.Var x, dx)::List.remove_assoc (Logic.Var x) env.doms
      ; model = (x, v)::env.model
      ; vlist = xs
      }
)

let choice = function
  | [] -> fail "no choice"
  | x::xs -> List.fold_left (<|>) x xs

let rec extract_model (n : int) (f : Logic.atom list) = step @@ fun {vlist; model; _} ->
  let next = extract_model (n/2) f in
  let next_decision = extract_model (n - 1) f in
  if n = 0 then abort
  else match vlist with
  | [] ->
    if Checker.check_list model f then "found model !" <?> return model
    else fail "extract model"
  | x::_ ->
    let* dx = eval (Var x) in
    match Interval.split dx with
    | Single v -> decide v <&> next
    | Split (d1, d2) ->
      let c1, c2 = Interval.peek d1, Interval.peek d2 in
      choice [
        decide c1 <&> next_decision;
        decide c2 <&> next_decision;
        update_dom (Var x) d1 <&> propagate f <&> next;
        update_dom (Var x) d2 <&> propagate f <&> next
      ]

let generic_solver (p : Logic.atom list) =
  propagate p <&> debug <&> extract_model 64 p

let solve p =
  let vlist = Logic.lvars p in
  let doms = [] in
  let n = 10 in
  let model = [] in
  match run (generic_solver p) {n; doms; model; vlist} with
  | Fail _ -> Model.UNSAT
  | Value m -> Model.SAT m
  | _ -> Last_effort.find_model p
end