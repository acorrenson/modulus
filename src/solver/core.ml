open Sat
open Logic
open Lstream
open Qf_lia

let dpllt f =
  let (cnf, vmap) = to_cnf f in
  let call_theory th atms =
    th (List.map (Hashtbl.find vmap) atms)
  in
  solve_all cnf
  |> map (call_theory lia)
  |> find_first is_sat ~default:UNSAT

let answer_to_string = function
  | SAT _ -> "sat"
  | UNSAT -> "unsat"
  | UNKNOWN -> "unknown"
