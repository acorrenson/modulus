open Logic
open Model

let check_atom (e : t) (atm : atom) : bool =
  Option.value ~default:false begin
    match atm with
    | Eq (t1, t2) ->
      let* v1 = eval_term t1 e in
      let* v2 = eval_term t2 e in
      Some (v1 = v2)
    end

let check_list (e : t) (l : atom list) : bool =
  List.for_all (check_atom e) l

let rec check (e : t) (f : formula) : bool =
  match f with
  | Atom a        -> check_atom e a
  | Or (f1, f2)   -> (check e f1 || check e f2)
  | And (f1, f2)  -> (check e f1 && check e f2)
  | Neg f         -> not (check e f)