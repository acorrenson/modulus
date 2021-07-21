(* open Modulus_lib.Qf_lia *)
(* open Modulus_lib.Smtlib *)
open Modulus_lib.Loop

let () =
  exec ()
  (* parse Sys.argv.(1) |> lia |> function
  | SAT -> print_endline "SAT"
  | UNSAT -> print_endline "UNSAT"
  | UNKNOWN -> print_endline "UNKNOWN" *)
  