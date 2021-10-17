module Make(N: Logic.LIA_NUM) = struct
  open Logic.Make(N)
  open Sexp.Make(N)

type smt_logic =
  | ALL
  | QF_LIA

type command =
  | Assert of formula
  | SetLogic of smt_logic
  | GetModel
  | Exit
  | CheckSat
  | DeclareConst of string * ttype

type smtscript = command list

let rec make_term (s : sexp) : term =
  match s with
  | Sym v -> Var v
  | Int i -> Cst i
  | Cons [Sym "+"; t1; t2] -> Add (make_term t1, make_term t2)
  | _ -> failwith "invalid term"


let rec make_formula (s : sexp) : formula =
  match s with
  | Sym _ -> failwith "a symbol is not a formula"
  | Cons [Sym "not"; f] -> Neg (make_formula f)
  | Cons [Sym "and"; f; f'] -> And (make_formula f, make_formula f')
  | Cons [Sym "or"; f; f'] -> Or (make_formula f, make_formula f')
  | Cons [Sym "="; a; b] -> Atom (Eq (make_term a, make_term b))
  | _ -> failwith "invalid formula"


let make_command s args =
  match s with
  | "set-logic" -> begin
    match args with
    | [Sym "QF_LIA"] -> SetLogic QF_LIA
    | [Sym "ALL"] -> SetLogic ALL
    | _ -> failwith "invalid arguments for command 'set-logic'"
    end
  | "assert" -> begin
    match args with
    | [e] -> Assert (make_formula e)
    | _ -> failwith "invalid arguments for command 'assert'"
    end
  | "exit" -> Exit
  | "check-sat" -> CheckSat
  | "get-model" -> GetModel
  | "declare-const" -> begin
    match args with
    | [Sym x; Sym "Int"] -> DeclareConst (x, Int)
    | _ -> failwith "invalid constant declaration (the only supported type is 'Int')"
    end
  | _ -> failwith ("invalid command " ^ s)


let command_of_sexp s =
  match s with
  | Cons (Sym s::args) ->
    make_command s args
  | _ -> failwith "invalid command"

let smt_of_sexp = List.map command_of_sexp

type typing_env = (string, ttype) Hashtbl.t

exception VarDup of string
exception NoType of string

let rec type_check_term (t : term) (e : typing_env) =
  match t with
  | Var x -> begin
    match Hashtbl.find_opt e x with
    | Some Int -> ()
    | _ -> raise (NoType x)
    end
  | Cst _ -> ()
  | Add (a, b) -> type_check_term a e; type_check_term b e

let rec type_check_formula (f : formula) (e : typing_env) =
  match f with
  | And (f1, f2) -> type_check_formula f1 e; type_check_formula f2 e
  | Or (f1, f2) -> type_check_formula f1 e; type_check_formula f2 e
  | Neg f' -> type_check_formula f' e
  | Atom (Eq (a, b)) -> type_check_term a e; type_check_term b e

let type_check_command (c : command) (e : typing_env) =
  match c with
  | Assert f -> type_check_formula f e
  | DeclareConst (x, t) -> begin
    match Hashtbl.find_opt e x with
    | Some _ -> raise (VarDup x)
    | None -> Hashtbl.add e x t
    end
  | _ -> ()

let type_check s =
  let e : typing_env = Hashtbl.create 10 in
  List.iter (fun c -> type_check_command c e) s
end