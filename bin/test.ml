open Modulus_lib.Loop
open Modulus_lib.Qf_lia

let test_file sat f =
  let ctx = batch f in
  try if sat then
    assert (is_sat ctx.model)
  else
    assert (ctx.model = UNSAT)
  with _ -> failwith f

let () =
  let dir_sat = "./examples/sat/" in
  let dir_unsat = "./examples/unsat/" in
  Array.iter (fun fn -> test_file true (dir_sat ^ fn)) (Sys.readdir dir_sat);
  Array.iter (fun fn -> test_file false (dir_unsat ^ fn)) (Sys.readdir dir_unsat)




(* let ensure_sat msg (a : atom) =
  Printf.printf "[sat] %s\n" msg;
  assert (is_sat (lia1 a))

let ensure_unsat msg (a : atom) =
  Printf.printf "[unsat] %s\n" msg;
  assert (lia1 a = UNSAT)

let () =
  ensure_unsat "0 != 1" (Eq (Cst 0, Cst 1));
  ensure_sat "1 = 1" (Eq (Cst 1, Cst 1));
  ensure_sat "x = x" (Eq (Var "x", Var "x"));
  ensure_sat "x = y" (Eq (Var "x", Var "y")); *)