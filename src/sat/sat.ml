type clause = int list

type cnf = clause list

let remove_lit l = List.filter ((=) l)

let rec propagate (l : int) (problem : cnf) =
  match problem with
  | [] -> []
  | c::cs ->
    if List.mem l c
    then propagate l cs
    else (remove_lit (-l) c)::propagate l cs

let rec solve (problem : cnf) =
  match problem with
  | [] -> Some []
  | []::_ -> None
  | (l::ls)::cs ->
    match solve (propagate l cs) with
    | Some _ as sol -> sol
    | None -> solve (propagate (-l) (ls::cs))

let rec solve_all (problem : cnf) =
  match solve problem with
  | None -> Lstream.Nil
  | Some s -> Cons (s, lazy (solve_all (List.map Int.neg s::problem)))