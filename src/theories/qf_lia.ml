(**
  {1 Lia}
  The Modulus internal solver for LIA

  Disclaimer : this module is a "work in progress"
*)
open Logic
open Model

(**
  Solve a single arithmetic goal
*)
let lia1 (a : atom) : anwser =
  match a with
  | Eq (Var x, Var y) -> SAT (fun z -> if z = x || z = y then Some Bigint.zero else None)
  | Eq (Cst x, Cst y) ->
    if x = y then SAT (fun _ -> None) else UNSAT
  | Eq (Var x, Cst a)
  | Eq (Cst a, Var x) -> SAT (fun y -> if x = y then Some a else None)
  | _ -> UNKNOWN

let debug m x = match m x with Some x -> string_of_int x | _ -> "?"

(**
  Solve a list of arithmetic goals under the hypothesis
  that goals are independants (don't share variables)
*)
let rec lia_indep (l : atom list) : anwser =
  match l with
  | [] -> SAT (fun _ -> None)
  | eq::eqs ->
    match lia1 eq with
    | SAT m ->
      begin match lia_indep eqs with
      | SAT m' ->
        SAT (merge_model m m')
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
  else UNKNOWN

let is_sat = function SAT _ -> true | _ -> false