open Modulus_lib.Logic
open Modulus_lib.Lia

let ensure_sat msg (a : atom) =
  Printf.printf "[sat] %s\n" msg;
  assert (lia1 a = SAT)

let ensure_unsat msg (a : atom) =
  Printf.printf "[unsat] %s\n" msg;
  assert (lia1 a = UNSAT)

let () =
  ensure_unsat "0 != 1" (Eq (Cst 0, Cst 1));
  ensure_sat "1 = 1" (Eq (Cst 1, Cst 1));
  ensure_sat "x0 = x0" (Eq (Var 0, Var 0));
  ensure_sat "x0 = x1" (Eq (Var 0, Var 1))