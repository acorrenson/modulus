open Sat
open Logic
open Model
open Lstream
open Qf_lia

let find_first_sat l =
  let rec step all_unsat l =
    match Lazy.force l with
    | Nil -> if all_unsat then UNSAT else UNKNOWN
    | Cons (x, xs) ->
      begin match x with
      | SAT _ -> x
      | UNSAT -> step all_unsat xs
      | UNKNOWN -> step false xs
      end
  in step true l

let dpllt f =
  let (cnf, vmap) = to_cnf f in
  let call_theory th atom_ids =
    let atoms = List.map (fun id ->
      let a = Hashtbl.find vmap (abs id) in
      if id < 0 then neg_atom a else a
    ) atom_ids in
    th atoms
  in
  solve_all cnf
  |> map (call_theory MiniLia.solve)
  |> find_first_sat
  |> function
  | SAT m as res -> assert (check f m = Some true); res
  | _ as res -> res

let answer_to_string = function
  | SAT _ -> "sat"
  | UNSAT -> "unsat"
  | UNKNOWN -> "unknown"
