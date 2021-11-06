type clause = int list

type cnf = clause list

module S = Strategy.Make(struct type t = cnf end)

open Strategy
open S
open Minicat.Monad.Make(S)
open Minicat.Alternative.Make(S)

let string_of_clause c =
  "{" ^ String.concat " " (List.map string_of_int c) ^ "}"

let string_of_cnf c =
  "{" ^ String.concat " " (List.map string_of_clause c) ^ "}"

let remove_lit (l : int) = List.filter ((<>) l)

let propagate l =
  update @@ List.filter_map (fun c ->
    if List.mem l c then None
    else Some (remove_lit (-l) c)
  )

let add l = fun ls -> l::ls

type 'a solver = 'a t

let solve = ffix @@ fun recall cnf ->
  match cnf with
  | [] -> return []
  | []::_ -> fail "unsat"
  | (l::_)::_ ->
    (propagate l <&> recall |> map (add l))
    <|>
    (propagate (-l) <&> recall |> map (add (-l)))