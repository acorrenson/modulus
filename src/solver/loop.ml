open Logic
open Core

type smt_state =
  | Start_mode
  | Assert_mode
  | Sat_mode
  | Unsat_mode

type context = {
  tenv  : Scripts.typing_env;
  state : smt_state;
  stack : formula list;
  logic : Scripts.smt_logic;
  model : Qf_lia.anwser;
}

let do_assert ctx f =
  match ctx.state with
  | Start_mode ->
    Printf.eprintf "(error \"logic is not set\")\n"; ctx
  | _ ->
    try
      Scripts.type_check_formula f ctx.tenv;
      Printf.eprintf "success\n";
      { ctx with stack = (f::ctx.stack); state = Assert_mode }
    with
      | Scripts.VarDup x ->
        Printf.eprintf "(error \"variable %s already declared\")\n" x; ctx
      | Scripts.NoType x ->
        Printf.eprintf "(error \"variable %s is not declared\")\n" x; ctx

let do_set_logic ctx l =
  match ctx.state with
  | Start_mode -> 
    Printf.eprintf "success\n";
    { ctx with logic = l; state = Assert_mode }
  | _ ->
    Printf.eprintf "(error \"logic can\'t be set now\")\n"; ctx

let do_declare_const ctx x t =
  match ctx.state with
  | Start_mode ->
    Printf.eprintf "(error \"logic is not set\")\n"; ctx
  | _ ->
    match Hashtbl.find_opt ctx.tenv x with
    | Some _ -> raise (Scripts.VarDup x)
    | None ->
      Printf.eprintf "success\n";
      Hashtbl.add ctx.tenv x t; ctx

let do_check_sat ctx =
  match ctx.state with
  | Start_mode ->
    Printf.eprintf "(error \"nothing to check\")\n"; ctx
  | _ ->
    let sat m =
      Printf.eprintf "sat\n"; { ctx with state = Sat_mode; model = m } in
    let unsat () =
      Printf.eprintf "unsat\n"; { ctx with state = Unsat_mode } in
    let unknown () =
      Printf.eprintf "unknown\n"; { ctx with state = Sat_mode } in
    match ctx.stack with
    | [] -> sat (SAT (fun _ -> None))
    | l::ls ->
      let f = List.fold_left (fun acc f -> And (acc, f)) l ls in
      match dpllt f with
      | SAT _ as m -> sat m
      | UNSAT -> unsat ()
      | UNKNOWN -> unknown ()

let do_get_model ctx =
  match ctx.state with
  | Sat_mode ->
    let vars = List.fold_left VSet.union VSet.empty (List.map Logic.vars ctx.stack) in
    VSet.iter (fun x ->
      match ctx.model with
      | SAT m ->
        Printf.eprintf "(%s %d)\n" x (Option.get (m x))
      | UNKNOWN -> ()
      | UNSAT -> assert false
    ) vars;
    { ctx with state = Sat_mode }
  | _ ->
    Printf.eprintf "(error \"not in sat mode\")\n"; ctx

let exec_one ctx sxp =
  begin try
    match Scripts.command_of_sexp sxp with
    | Assert f -> do_assert ctx f
    | SetLogic l -> do_set_logic ctx l
    | DeclareConst (x, t) -> do_declare_const ctx x t
    | GetModel -> do_get_model ctx
    | CheckSat -> do_check_sat ctx
    | Exit -> raise Exit
  with
    | Failure msg ->
      Printf.eprintf "(error \"%s\")\n" msg;
      ctx
    | Exit -> raise Exit
  end

let batch f =
  match Sexp.of_file f with
  | Some (sxp, Lstream.Nil) ->
    List.fold_left exec_one {
      tenv = Hashtbl.create 50;
      state = Start_mode;
      stack = [];
      logic = ALL;
      model = UNKNOWN;
    } sxp
  | _ -> Printf.eprintf "parse error\n"; exit 1

let repl () =
  let rec step ctx =
    flush stderr;
    Printf.printf "> ";
    match Sexp.of_string (read_line ()) with
    | None | Some ([], _) | Some (_::_::_, _) ->
      Printf.eprintf "(error \"this command is incorrect\")\n";
      step ctx
    | Some ([s], _) ->
      try step (exec_one ctx s)
      with Exit -> exit 0
  in
  step {
    tenv = Hashtbl.create 50;
    state = Start_mode;
    stack = [];
    logic = ALL;
    model = UNKNOWN;
  }