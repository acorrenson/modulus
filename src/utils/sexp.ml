open Lstream
open Parsing

type sexp =
  | Sym of string
  | Int of int
  | Cons of sexp list

let lex s = Genlex.make_lexer ["("; ")"] s |> of_stream

let is_atom = function
  | Genlex.Int _ | Genlex.Ident _ -> true
  | _ -> false

let parens p =
  satisfy (function Genlex.Kwd "(" -> true | _ -> false)
  >> p <<
  satisfy (function Genlex.Kwd ")" -> true | _ -> false)

let rec parse_sexp i =
  begin
    satisfy is_atom => (function Genlex.Int i -> Int i | Genlex.Ident x -> Sym x | _ -> assert false)
    <|>
    parens (many parse_sexp => (fun r -> Cons r))
  end i


let of_string s =
  Stream.of_string s |> lex |> parse_sexp


(* "(a b 1)" -> Kwd "("; Ident "a"; Ident "a"; Int 1; Kwd ")" *)