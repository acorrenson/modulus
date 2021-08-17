(**
  {1 Lia}
  The Modulus internal solver for LIA

  Disclaimer : this module is a "work in progress"
*)

open Logic

module Models = struct
  type value = int

  type model = string -> value option
  
  let (let*) = Option.bind

  let rec eval_term (t : term) (e : model) : value option =
    match t with
    | Cst i -> Some i
    | Var x -> e x
    | Add (t1, t2) ->
      let* v1 = eval_term t1 e in
      let* v2 = eval_term t2 e in
      Some (v1 + v2)
  
  let check_atom (atm : atom) (e : model) : bool option =
    match atm with
    | Eq (t1, t2) ->
      let* v1 = eval_term t1 e in
      let* v2 = eval_term t2 e in
      Some (v1 = v2)
  
  let rec check (f : formula) (e : model) : bool option =
    match f with
    | Atom a -> check_atom a e
    | Or (f1, f2) ->
      let* c1 = check f1 e in
      let* c2 = check f2 e in
      Some (c1 || c2)
    | And (f1, f2) ->
      let* c1 = check f1 e in
      let* c2 = check f2 e in
      Some (c1 && c2)
    | Neg f ->
      let* c = check f e in
      Some (not c)

  let update_model (m : model) (x : string) (v : value) =
    fun y -> if y = x then Some v else m y
  
  let merge_model (m : model) (m' : model) =
    fun x -> match m x with Some _ as v -> v | None -> m' x
end

type anwser = SAT of Models.model | UNSAT | UNKNOWN


(**
  Solve a single arithmetic goal
*)
let lia1 (a : atom) : anwser =
  match a with
  | Eq (Var x, Var y) -> SAT (fun z -> if z = x || z = y then Some 0 else None)
  | Eq (Cst x, Cst y) ->
    if x = y then SAT (fun _ -> None) else UNSAT
  | Eq (Var x, Cst a)
  | Eq (Cst a, Var x) -> SAT (fun y -> if x = y then Some a else None)
  | _ -> UNKNOWN


let maybe_sat r =
  match r with
  | SAT _ -> UNKNOWN
  | _ -> r

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
      | SAT m' -> SAT (Models.merge_model m m')
      | _ as r -> r
      end
    | UNSAT -> UNSAT
    | UNKNOWN -> maybe_sat (lia_indep eqs)

let lia (l : atom list) : anwser =
  if are_independants l then
    lia_indep l
  else UNKNOWN

let is_sat = function SAT _ -> true | _ -> false