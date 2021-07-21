open Logic

type smt_state =
  | Start_mode
  | Assert_mode
  | Sat_mode
  | Unsat_mode

type context = {
  tenv  : Scripts.typing_env;
  state : smt_state;
  stack : formula list;
  logic : Scripts.smtlogic;
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
    Printf.eprintf "unknown\n";
    { ctx with state = Sat_mode }

let do_get_model ctx =
  match ctx.state with
  | Sat_mode ->
    Printf.eprintf "unsupported\n";
    { ctx with state = Sat_mode }
  | _ ->
    Printf.eprintf "(error \"not in sat mode\")\n"; ctx


let exec () =
  let rec step ctx =
    flush stderr;
    Printf.printf "> ";
    match Sexp.of_string (read_line ()) with
    | None -> Printf.eprintf "(error \"this command is incorrect or unsupported\")\n"
    | Some ([s], _) ->
      begin try
        match Scripts.command_of_sexp s with
        | Assert f -> step (do_assert ctx f)
        | SetLogic l -> step (do_set_logic ctx l)
        | DeclareConst (x, t) -> step (do_declare_const ctx x t)
        | GetModel -> step (do_get_model ctx)
        | CheckSat -> step (do_check_sat ctx)
        | Exit -> ()
      with
        | Failure msg -> Printf.eprintf "(error \"%s\")\n" msg
        | _ -> assert false
      end
    | _ -> ()
  in
  step {
    tenv = Hashtbl.create 50;
    state = Start_mode;
    stack = [];
    logic = ALL;
  }