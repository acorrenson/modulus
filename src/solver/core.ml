open Sat
open Logic
open Model
open Lstream
open Qf_lia

let find_first_sat l =
  let rec step all_unknown l =
    match Lazy.force l with
    | Nil -> if all_unknown then UNKNOWN else UNSAT
    | Cons (x, xs) ->
      begin match x with
      | SAT _ -> x
      | UNSAT -> step false xs
      | UNKNOWN -> step all_unknown xs
      end
  in step true l

let dpllt f =
  let (cnf, vmap) = to_cnf f in
  let call_theory th atms =
    th (List.map (Hashtbl.find vmap) atms)
  in
  solve_all cnf
  |> map (call_theory QfLia.solve)
  |> find_first_sat
  |> function
  | SAT m as res -> assert (check f m = Some true); res
  | _ as res -> res

let answer_to_string = function
  | SAT _ -> "sat"
  | UNSAT -> "unsat"
  | UNKNOWN -> "unknown"
