open Modulus_lib.Logic
open Modulus_lib.Lia


let test () = 
  match lia1 (Eq(Var 0, Cst 1)) with
  | SAT -> print_endline "SAT"
  | UNSAT -> print_endline "UNSAT"
  | _ -> print_endline "UNKNOWN"

let () = test ()