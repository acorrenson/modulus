open Logic
open Sexp

type smtlogic =
  | ALL
  | LIA

type command =
  | Assert of formula
  | SetLogic of smtlogic
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
    | [Sym "LIA"] -> SetLogic LIA
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



