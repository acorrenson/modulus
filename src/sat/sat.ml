type clause = int list

type cnf = clause list

let remove_lit l = List.filter ((<>) l)

let string_of_clause c =
  "{" ^ String.concat " " (List.map string_of_int c) ^ "}"

let string_of_cnf c =
  "{" ^ String.concat " " (List.map string_of_clause c) ^ "}"

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
    | Some s -> Some (l::s)
    | None ->
      if (List.compare_length_with ls 0 = 0) then None
      else Option.map (fun s -> -l::s) @@ solve @@ propagate (-l) (ls::cs)

let rec solve_all (problem : cnf) =
  lazy begin
    match solve problem with
    | None -> Lstream.Nil
    | Some s -> Lstream.Cons (s, solve_all (List.map Int.neg s::problem))
  end