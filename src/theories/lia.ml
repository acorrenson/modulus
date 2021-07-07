(**
  {1 Lia}
  The Modulus internal solver for LIA

  Disclaimer : this module is a "work in progress"
*)

open Logic

type anwser = SAT | UNSAT | UNKNOWN

(** 
  Solve a single arithmetic goal
*)
let lia1 (a : atom) : anwser =
  match a with
  | Eq(Var _, Var _) -> SAT
  | Eq(Cst x, Cst y) -> 
    if x = y then  SAT  else  UNSAT
  | Eq(Var _, Cst _)
  | Eq(Cst _, Var _) -> SAT
  | _ -> UNKNOWN


let maybe_sat r =
  match r with
  | SAT -> UNKNOWN
  | _ -> r

(**
  Solve a list of arithmetic goals under the hypothesis
  that goals are independants (don't share variables)
*)
let rec lia_indep (l : atom list) : anwser =
  match l with
  | [] -> SAT
  | eq::eqs ->
    match lia1 eq with
    | SAT -> lia_indep eqs
    | UNSAT -> UNSAT
    | UNKNOWN -> maybe_sat (lia_indep eqs)

let lia (l : atom list) : anwser =
  if are_independants l then
    lia_indep l
  else UNKNOWN