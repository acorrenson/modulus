(**
  This module provides utilities to define new resolution strategies.
  We introduce a monad [Solver] to build customized solvers or generate
  new ones given a domain (see {!Domain}).
*)

open Logic
open Checker
open Domain

(**
  !!! THIS MODULE IS A WORK IN PROGRESS !!!

  A generic evaluator for ground terms (see {!Logic.term}).
  This module is designed to assist the programmers willing to
  extend modulus solver.

  If you want add a solver for a specific theory (see {!Solver}),
  this solver will require a custom evaluator.

  An evaluation function [eval] is any function associating a value to
  a term for a given model (see {!Model.t}).
  If a custom evaluator is not intended to evaluate a specific expression [e],
  then [eval e m] should return [None] for any model [m].
*)
module type Evaluator = sig
  val eval : term -> Model.t -> int option
end

exception Contradiction of string
exception Aborted

module Make (D : Domain) : sig
  type state = (term * D.t) list
  (** Internal state of the solver *)

  type 'a status
  (**
    Status of the solver after an update of the internal state.
    The status is either:
    {ul
      {- A failure (reported if the search space becomes empty)}
      {- A simple value (reported if the search space is not updated)}
      {- An update notification (reported if the search space has been updated)}
      {- An interuption of the resolution}
    }
  *)

  val get : 'a status -> 'a
  (**
    Get the a [value] from a status report.
    If the reported status is a failure or an interruption
    this may raise either a [Contradiction] exception (in case of a failure)
    or a [Aborted] exception (in case of an interruption).
  *)

  type 'a update
  (** A monad to represent functions updating the solver state *)

  val run : 'a update -> state -> 'a status
  (**
    Execute an update function on a given state
  *)

  val return : 'a -> 'a update
  (**
    [return v] is an update function ignoring the state
    of the solver and returning the value [v]
  *)

  val update : term -> D.t -> D.t update
  (**
    [update t v] is an update function
    which rebinds the value [v] to the term [t] in
    the solver's state and return this new value.

    If [t] is already bound, intersection as defined by [D.inter] is performed
    to unify the domains. Such an operation may report a failure if
    the intersection results in [D.bot] !
  *)

  val fail : string -> 'a update
  (**
    [fail msg] is an update function always resulting in a failure
    with associated error message [msg].
  *)

  val leak : state update
  (**
    [leak] is an update functions providing the current state of the solver
    as a return value.
  *)

  val (>>) : 'a update -> 'b update -> 'b update
  (**
    if [f] and [g] are 2 update functions, [f >> g] is the update
    function performing first [f] and then [g].
  *)

  val (<|>) : 'a update -> 'a update -> 'a update
  (**
    if [f] and [g] are 2 update functions, [f <|> g] is an update function
    trying first to apply [f] and fallbacks to [g] if [f] failed.
  *)

  (** TODO : fix operator *)
  (* val fix : 'a update -> 'a update *)

  val generic_solver : atom list -> Model.t update
  (**
    A generic solver generated using the [update] monad.
  *)

  val solve : atom list -> Model.answer
  (**
    Ready to be used version of [generic_solver].
  *)
end = struct

type state = (term * D.t) list

type 'a status =
  | Value of 'a
  | Update of state * 'a
  | Fail of string
  | Abort

type 'a update = state -> 'a status

let[@inline] run f s = f s

let return (v : 'a) : 'a update = fun _ -> Value v

let update (t : term) (v : 'a) : 'a update = fun s ->
  match List.assoc_opt t s with
  | Some v' ->
    if v = v' then Value v' else
    let d = D.inter v v' in
    if D.is_empty d then
      Fail (Format.asprintf "update failed %a ∩ %a = %a"
        D.pp_print v D.pp_print v' D.pp_print d)
    else Update ((t, d)::List.remove_assoc t s, d)
  | None ->
    Update ((t, v)::s, v)

let get (s : 'a status) : 'a =
  match s with
  | Abort -> raise Aborted
  | Fail l -> raise (Contradiction l)
  | Value v -> v
  | Update (_, v) -> v

let leak : state update = fun s -> Value s

let fail msg = fun _ -> Fail msg

let abort = fun _ -> Abort

let (let*) (m : 'a update) (f : 'a -> 'b update) : 'b update = fun s ->
  match m s with
  | Abort         -> Abort
  | Fail _ as err -> err
  | Value v       -> f v s
  | Update (e, v) ->
    match f v e with
    | Value v -> Update (e, v)
    | _ as ret -> ret

let rec eval (x : term) : D.t update = fun s ->
  match List.assoc_opt x s with
  | Some v -> Value v
  | None ->
    match x with
    | Var _ -> update x D.top s
    | Cst v -> update x (D.singleton v) s
    | Add (t1, t2) ->
      begin
        let* v1 = eval t1 in
        let* v2 = eval t2 in
        update x (D.add v1 v2)
      end s

let (>>) (f : 'a update) (g : 'b update) : 'b update =
  let* _ = f in g

let (<|>) (u1 : 'a update) (u2 : 'b update) : 'b update = fun s ->
  match u1 s with
  | Fail _ -> u2 s
  | _ as r -> r

let propagate_one (Eq (t1, t2) : atom) : unit update =
  let* d1 = eval t1 in
  let* d2 = eval t2 in
  let d = D.inter d1 d2 in
  update t1 d >> update t2 d >> return ()

let propagate_one_backward (Eq (t1, t2) : atom) : unit update =
  let rec step (t : term) (dt : D.t) : unit update =
    match t with
    | Cst _ | Var _ -> update t dt >> return ()
    | Add (t1, t2) ->
      let* d1 = eval t1 in
      let* d2 = eval t2 in
      let (d1', d2') = D.add_inv d1 d2 dt in
      step t1 d1' >> step t2 d2' >> return ()
  in
  let* d1 = eval t1 in
  let* d2 = eval t2 in
  step t1 d1 >> step t2 d2

let sequence (l : 'a list) (p : 'a -> unit update) : unit update =
  List.fold_left (>>) (return ()) (List.map p l)

let propagate l : unit update =
  sequence l propagate_one
  >> sequence l propagate_one_backward

let extract_model (p : atom list) =
  let rec step depth vlist (model : Model.t) : Model.t update =
    if depth <= 0 then abort else
    match vlist with
    | [] ->
      if check_list model p
      then return model
      else fail "extract model"
    | x::xs ->
      let* dx = eval (Var x) in
      let decide x v = update (Var x) (D.singleton v) >> propagate p in
      match D.split dx with
      | Split (d1, d2) ->
        let c1, c2 = D.peek d1, D.peek d2 in
        (decide x c1 >> (step (depth - 1) xs ((x, c1)::model)))
        <|>
        (decide x c2 >> (step (depth - 1) xs ((x, c2)::model)))
        <|>
        (update (Var x) d1 >> propagate p >> step (depth - 1) vlist model)
        <|>
        (update (Var x) d2 >> propagate p >> step (depth - 1) vlist model)
      | Single v ->
        step (depth - 1) xs ((x, v)::model)
    in
    step 512 (lvars p) []

let generic_solver (p : atom list) =
  propagate p >> extract_model p

let solve (p : atom list) =
  let go = propagate p >> extract_model p in
  try Model.SAT (get (go []))
  with
  | Aborted -> Model.UNKNOWN
  | Contradiction _ -> Model.UNSAT

end