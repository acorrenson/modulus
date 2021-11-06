open Modulus_lib
open Strategy
open Monadsat

let () = Random.self_init ()

let random_list () =
  let len = Random.int 20 in
  List.init len (fun _ -> 20 - Random.int 20)

let random_cnf () =
  let len = Random.int 50 in
  List.init len (fun _ -> random_list ())

let () =
  let print_result = function
    | Some m  -> Printf.printf "  \x1b[32mSAT\x1b[0m : %s\n" (string_of_clause m)
    | None    -> Printf.printf "  \x1b[31mUNSAT\x1b[0m\n"
  in
  List.init 1000 (fun _ -> random_cnf ()) |>
  List.iteri (fun i cnf ->
    Printf.printf "testing unit [%d]\n" i;
    let r1 = Runtime.time (fun () -> Sat.solve cnf) in
    let r2 = Runtime.time (fun () -> run_opt solve cnf) in
    assert (r1.result = r2.result);
    Printf.printf "  (old) solver result [runtime : \x1b[33m%f\x1b[0m] : \n" r1.time;
    print_result r1.result;
    Printf.printf "  (new) solver result [runtime : \x1b[33m%f\x1b[0m] : \n" r2.time;
    print_result r2.result;
  )