(**
  {1 Lia}
  The Modulus internal solver for LIA

  Disclaimer : this module is a "work in progress"
*)

open Logic
open Model

module Interval = struct
  type t = { lo : int; hi : int }

  let _add_int x y =
    if x / 2 + y / 2 > max_int / 2 then
      max_int
    else if x / 2 + y / 2 < min_int / 2 then 
      min_int
    else
      x + y

  let add (i1 : t) (i2 : t) : t =
    { lo = _add_int i1.lo i2.lo; hi = _add_int i1.hi i2.hi }

  let sub (i1 : t) (i2 : t) : t =
    { lo = _add_int i1.lo (-i2.hi); hi = _add_int i1.hi (-i2.lo) }
  
  let inter (i1 : t) (i2 : t) : t =
    { lo = max i1.lo i2.lo; hi = min i1.hi i2.hi }
  
  let is_empty ({ lo; hi }) : bool = lo > hi

  let top : t = { lo = min_int; hi = max_int }

  let singleton (x : int) : t = { lo = x; hi = x}

  let split ({lo; hi} : t) =
    if lo = hi then `Single lo
    else
      let mid = (lo + hi) / 2 in
      `Pair ({ lo; hi = mid }, { lo = mid; hi})
end

module Solver = struct
  type state = (term * Interval.t) list

  type 'a status =
    | Value of 'a
    | Update of state * 'a
    | Fail of string

  type 'a update = state -> 'a status

  let return (x : 'a) : 'a update = fun _ -> Value x

  let update t x : 'a update = fun s -> Update ((t, x)::s, x)

  exception UpdateFail of string

  let get (s : 'a status) : 'a =
    match s with
    | Fail l -> raise (UpdateFail ("nothing to get because: " ^ l))
    | Value v -> v
    | Update (_, v) -> v

  let (let*) (m : 'a update) (f : 'a -> 'b update) : 'b update = fun s ->
    match m s with
    | Fail _ as err -> err
    | Value v -> f v s
    | Update (e, v) ->
      match f v e with
      | Value v -> Update (e, v)
      | _ as ret -> ret

  let rec eval (x : term) : Interval.t update = fun s ->
    match List.assoc_opt x s with
    | Some v -> Value v
    | None ->
      match x with
      | Var _ -> update x Interval.top s
      | Cst v -> update x (Interval.singleton v) s
      | Add (t1, t2) ->
        begin
          let* v1 = eval t1 in
          let* v2 = eval t2 in
          update x (Interval.add v1 v2)
        end s

  exception Contradiction of string

  let (>>) (u1 : 'a update) (u2 : 'b update) : 'b update =
    let* _ = u1 in u2

  let leak : state update = fun s -> Value s

  let fail msg = fun _ -> Fail msg

  let propagate_one (Eq (t1, t2) : atom) : unit update =
    let* d1 = eval t1 in
    let* d2 = eval t2 in
    let d = Interval.inter d1 d2 in
    if Interval.is_empty d then fail "propagate one"
    else update t1 d >> update t2 d >> return ()

  let sequence (l : 'a list) (p : 'a -> unit update) : unit update =
    List.fold_left (>>) (return ()) (List.map p l)

  let propagate l : unit update =
    sequence l propagate_one

  let extract_model (p : atom list) =
    let vars =
        List.map avars p
        |> List.fold_left VSet.union VSet.empty
        |> VSet.to_seq
        |> List.of_seq
    in
    let rec step vlist (model : Model.t) : Model.t update =
      match vlist with
      | [] ->
        if List.for_all (fun a -> Option.get (check_atom a model)) p
        then return model
        else fail "extract model"
      | x::xs ->
        let* dx = eval (Var x) in
        match Interval.split dx with
        | `Pair (d1, d2) ->
          begin try
            update (Var x) d1 >> step vlist model
          with Contradiction _ ->
            update (Var x) d2 >> step vlist model
          end
        | `Single v -> step xs ((x, v)::model)
      in
      step vars []
  
  let solve (p : atom list) =
    let go = propagate p >> extract_model p in
    get (go [])
end

(**
  Solve a single arithmetic goal
*)
let lia1 (a : atom) : anwser =
  match a with
  | Eq (Var x, Var y) -> SAT [x, 0; y, 0]
  | Eq (Cst x, Cst y) ->
    if x = y then SAT [] else UNSAT
  | Eq (Var x, Cst a)
  | Eq (Cst a, Var x) -> SAT [x, a]
  | _ -> UNKNOWN

let debug m x = match m x with Some x -> string_of_int x | _ -> "?"

(**
  Solve a list of arithmetic goals under the hypothesis
  that goals are independants (don't share variables)
*)
let rec lia_indep (l : atom list) : anwser =
  match l with
  | [] -> SAT []
  | eq::eqs ->
    match lia1 eq with
    | SAT m ->
      begin match lia_indep eqs with
      | SAT m' ->
        SAT (m @ m')
      | _ as r -> r
      end
    | UNSAT -> UNSAT
    | UNKNOWN ->
      match lia_indep eqs with
      | SAT _ -> UNKNOWN
      | _ as r -> r

let lia (l : atom list) : anwser =
  if are_independants l then
    lia_indep l
  else
  try SAT (Solver.solve l)
  with (Solver.UpdateFail _) -> UNSAT

let is_sat = function SAT _ -> true | _ -> false

