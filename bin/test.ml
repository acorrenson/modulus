open Modulus_lib.Loop
open Modulus_lib.Model

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