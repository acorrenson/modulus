(**
  {1 Lia}
  
  Modulus internal solver for QF_LIA
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


(** Alias for {!Format.asprintf} *)
let info = Format.asprintf

(**
  A module describing the resolution environments
*)
module DomMap = struct

(** A context is a record including a mapping from terms to intervals,
  a list of unbounded variables and a partial model.
*)
type t = { doms : (Logic.term * Interval.t) list; vlist : string list; model : Model.t }

let rec pp_print fmt = function
  | [] -> ()
  | (x, d)::next ->
    Format.fprintf fmt "%a := %a, %a"
      Logic.pp_term x
      Interval.pp_print d
      pp_print next

(** [set_dom_aux x d] is a strategy which binds domain [d] to the term [x]
    assuming [x] was unbound. This strategy returns [d] as return value.
*)
let set_dom x d =
  let set_dom_aux x d env =
    { env with doms = (x, d)::env.doms }
  in update_ret d (set_dom_aux x d)

(** [eval x] is a strategy which evaluates the term [x] in the current environment.
    If [x] is not bound to a domain, [eval x] binds [x] to {!Interval.top} first.
*)
let rec eval (x : Logic.term) = step @@ fun env ->
  (* info "evaluating %a in ctx [%a...]" Logic.pp_term x pp_print env.doms <?> *)
  match List.assoc_opt x env.doms with
  | Some v ->
    (* info "find %a := %a" Logic.pp_term x Interval.pp_print v <?> *)
    return v
  | None ->
    match x with
    | Var _ ->
      (* info "set %a := %a" Logic.pp_term x Interval.pp_print (Interval.top) <?> *)
      set_dom x Interval.top
    | Cst v ->
      (* info "set %a := %a" Logic.pp_term x Interval.pp_print (Interval.singleton v) <?> *)
      set_dom x (Interval.singleton v)
    | Add (t1, t2) ->
      let* v1 = eval t1 in
      let* v2 = eval t2 in
      (* info "set %a := %a" Logic.pp_term x Interval.pp_print (Interval.add v1 v2) <?> *)
      set_dom x (Interval.add v1 v2)

(** [update_dom x d] is a strategy which update the domain of [x] with domain [d].
    If [x] is already bound to a domain [d'], the domain of [x] is set to be the intersection
    [Interval.inter d d']. If the intersection is {!Interval.bot}, the strategy fails.
*)
let update_dom x d =
  (* info "update %a := %a" Logic.pp_term x Interval.pp_print d <?> *)
  let* dx = eval x in
  let dx' = Interval.inter dx d in
  if Interval.is_empty dx' then
    fail "unsat"
  else update @@ fun env ->
    { env with doms = (x, d)::(List.remove_assoc x env.doms) }
end

(**
  A mini monadic solver for QF_LIA
*)
module MiniLia = struct

(** A solver is a strategy which operates on environments of type {!DomMap.t} and
  returns a {!Model.t}.
*)
type solver = (DomMap.t, Model.t) t
open DomMap

(** A strategy doing nothing *)
let no_update = update (Fun.id)

(** A strategy printing its current environment *)
let debug = step @@ fun env ->
  info "In ctx [%a...]" pp_print env.doms <?> no_update

(** A strategy performing backward propagation.
    [propagate_one_back t d] assumes that term [t] should evaluates to domain [d]
    and consequently propagates this information to all subterms of [t].
*)
let rec propagate_one_back t d =
  let open Logic in
  (* info "propagating backward %a := %a" Logic.pp_term t Interval.pp_print d <?> *)
  match t with
  | Cst _ | Var _ -> update_dom t d
  | Add (t1, t2) ->
    let* d1 = eval t1 in
    let* d2 = eval t2 in
    let (d1', d2') = Interval.add_inv d1 d2 d in
    propagate_one_back t1 d1' <&> propagate_one_back t2 d2'

(** A strategy performing constraint propagation.
    [propagate_one c] propagate the constraint [c] in the current
    environment, thus restraining the domains.
    It may fails if the constraints [c] is insatisfiable in the context.
*)
let propagate_one (a : Logic.atom) =
  (* info "propagating %a" Logic.pp_atom a <?> *)
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

(** [propagate f] is a strategy
*)
let propagate (f : Logic.atom list) =
  (* info "propagate" <?> *)
  List.fold_left (<&>) no_update (List.map propagate_one f)

let decide v = step (fun env ->
  let x = List.hd env.vlist in
  let xs = List.tl env.vlist in
  let dx = Interval.singleton v in
  (* info "decide %s := %d" x v <?> *)
  set { doms = (Logic.Var x, dx)::List.remove_assoc (Logic.Var x) env.doms
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
    if Checker.check_list model f then return model
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
  propagate p <&> extract_model 16 p

let solve p =
  let vlist = Logic.lvars p in
  let doms = [] in
  let model = [] in
  match run (generic_solver p) { doms; model; vlist } with
  | Fail _ -> Model.UNSAT
  | Value m -> Model.SAT m
  | _ -> Last_effort.find_model p
end