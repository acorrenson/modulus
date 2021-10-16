open Logic

type value = int

type t = (string * value) list

type anwser = SAT of t | UNSAT | UNKNOWN

let (let*) = Option.bind

let rec eval_term (t : term) (e : t) : value option =
  match t with
  | Cst i -> Some i
  | Var x -> List.assoc_opt x e
  | Add (t1, t2) ->
    let* v1 = eval_term t1 e in
    let* v2 = eval_term t2 e in
    Some (v1 + v2)

let check_atom (atm : atom) (e : t) : bool option =
  match atm with
  | Eq (t1, t2) ->
    let* v1 = eval_term t1 e in
    let* v2 = eval_term t2 e in
    Some (v1 = v2)

let rec check (f : formula) (e : t) : bool option =
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

let update_model (m : t) (x : string) (v : value) =
  (x, v)::List.remove_assoc x m