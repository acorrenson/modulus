open Logic

type loc = int

type sexp =
  | Slist of loc * sexp list
  | Ssym of loc * string
  | Sint of loc * int

let get_loc = function
  | Slist (l, _) -> l
  | Ssym (l, _) -> l
  | Sint (l, _) -> l

type command =
  | Assert of atom
  | GetModel
  | CheckSat
  | DeclareConst of string * ttype

type smt_script = command list

let term_of_sexp s =
  match s with
  | Sint (_, i) -> Cst i
  | _ ->
    failwith (Printf.sprintf "expected term @ line %d\n" (get_loc s))

let atom_of_sexp s =
  match s with
  | Slist (_, [Ssym (_, "="); x; y]) ->
    Eq (term_of_sexp x, term_of_sexp y)
  | Slist (l, Ssym (_, "=")::args) ->
    failwith (Printf.sprintf "'=' expects 2 arguments, got %d @ line %d\n" (List.length args) l)
  | _ ->
    failwith (Printf.sprintf "expected atom @ line %d\n" (get_loc s))

let command_of_sexpr s =
  match s with
  | Slist (_, Ssym (l, "assert")::args) ->
    if List.length args = 1 then Assert (atom_of_sexp (List.hd args))
    else
      failwith (Printf.sprintf "'assert' expects 1 argument @ line %d\n" l)
  | Slist (_, Ssym (l, "check-sat")::args) ->
    if List.length args = 0 then CheckSat
    else
      failwith (Printf.sprintf "'check-sat' expects 0 arguments @ line %d\n" l)
  | Slist (l, Ssym (_, c)::_) ->
    failwith (Printf.sprintf "unsupported command '%s' @ line %d\n" c l)
  | Ssym _ | Sint _ ->
    failwith (Printf.sprintf "a symbol is not a command (forgot '(' ')' ?) @ line %d\n" (get_loc s))
  | _ ->
    failwith (Printf.sprintf "invalid or insupported command @ line %d\n" (get_loc s))


let script_of_sexp s =
  match s with
  | Slist (_, l) ->
    List.map command_of_sexpr l
  | _ ->
    failwith (Printf.sprintf "a symbol is not a command (forgot '(' ')' ?) @ line %d\n" (get_loc s))

