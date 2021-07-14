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





(* type 'a parser = string -> ('a * string) option

let pred (p : char -> bool) : char parser = fun s ->
  if String.length s = 0 then None
  else if p s.[0] then Some (s.[0], String.sub s 1 (String.length s - 1))
  else None

let char (c : char) : char parser = pred ((=) c)

let alpha : char parser = pred (function 'a'..'z' | 'A'..'Z' -> true | _ -> false)

let digit : char parser = pred (function '0'..'9' -> true | _ -> false)



let rec many (p : 'a parser) (s : string) =
  match p s with
  | None -> Some ([], s)
  | Some (x, r) ->
    match many p r with
    | None -> Some ([x], r)
    | Some (xs, r) -> Some (x::xs, r)

let (>>=) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser = fun s ->
  match p s with
  | None -> None
  | Some (x, r) -> f x r

let return (x : 'a) : 'a parser = fun s -> Some (x, s)

let many1 (p : 'a parser) =
  p >>= (fun x -> many p >>= fun xs -> return (x::xs))

let (<|>) (p : 'a parser) (q : 'a parser) : 'a parser = fun s ->
  match p s with
  | None -> q s
  | _ as r -> r

let (=>) (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  p >>= (fun x -> return (f x))

let (>>) (p : 'a parser) (q : 'b parser) : 'b parser =
  p >>= (fun _ -> q)

let (<<) (p : 'a parser) (q : 'b parser) : 'a parser =
  p >>= (fun x -> q >>= fun _ -> return x)

let var : term parser = alpha >> digit => (fun x -> Var (int_of_char x - int_of_char '0'))

let parens (p : 'a parser) : 'a parser =
  char '(' >> p << char ')'

let blank = many1 (char ' ' <|> char '\n' <|> char '\t' <|> char '\r')

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let symbol s =
  match explode s with
  | [] -> failwith "empty symbol"
  | x::xs ->
    List.fold_left (>>) (char x) (List.map char xs)

let p_atom =
  parens (char '=' >> blank >> var << blank >>= fun x -> var >>= fun y -> return (Eq (x, y))) *)