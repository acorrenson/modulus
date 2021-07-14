open Modulus_lib.Lia
open Modulus_lib.Smtlib

let () =
  parse Sys.argv.(1) |> lia |> function
  | SAT -> print_endline "SAT"
  | UNSAT -> print_endline "UNSAT"
  | UNKNOWN -> print_endline "UNKNOWN"
  