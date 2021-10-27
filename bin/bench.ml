open Modulus_lib
open Monadsat
open SolverMonad

let () = Random.self_init ()

let random_list () =
  let len = Random.int 20 in
  List.init len (fun _ -> 20 - Random.int 20)

let random_cnf () =
  let len = Random.int 50 in
  List.init len (fun _ -> random_list ())

let () =
  let print_result = function
    | Some m  -> Printf.printf "  SAT : %s\n" (string_of_clause m)
    | None    -> Printf.printf "  UNSAT\n"
  in
  List.iteri (fun i cnf ->
    Printf.printf "testing unit [%d]\n" i;
    let r1 = Runtime.time (fun () -> Sat.solve cnf) in
    let r2 = Runtime.time (fun () -> run solve cnf) in
    assert (r1.result = r2.result);
    Printf.printf "  (old) solver result [runtime : %f] : \n" r1.time;
    print_result r1.result;
    Printf.printf "  (new) solver result [runtime : %f] : \n" r2.time;
    print_result r2.result;
  ) (List.init 1000 (fun _ -> random_cnf ()))