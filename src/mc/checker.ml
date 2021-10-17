open Logic
open Model

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