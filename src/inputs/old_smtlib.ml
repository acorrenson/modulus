open Logic
open Sexp

type command =
  | Assert of atom
  | GetModel
  | CheckSat
  | DeclareConst of string * ttype

type smt_script = command list

let atom_of_sexp s =
  match s with
  | Cons [Sym "="; Int i; Int j] -> Eq (Cst i, Cst j)
  | Cons [Sym "="; Sym x; Sym y] -> Eq (Var x, Var y)
  | Cons [Sym "="; Int i; Sym x] -> Eq (Cst i, Var x)
  | Cons [Sym "="; Sym x; Int i] -> Eq (Cst i, Var x)
  | _ -> failwith "invalid atom"


let rec get_assertions l =
  match l with
  | [Cons [Sym "check-sat"]] -> []
  | Cons (Sym "assert"::[a])::l' -> atom_of_sexp a::get_assertions l'
  | _ -> failwith "invalid assertion"


let parse file =
  match of_file file with
  | None -> failwith "parse error"
  | Some (res, _) ->
    match res with
    | Cons [Sym "set-logic"; Sym "ALL"]::sc ->
      get_assertions sc
    | _ -> failwith "invalid"