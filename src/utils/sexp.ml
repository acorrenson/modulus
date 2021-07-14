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

let exact s = satisfy ((=) s)
let set_logic =
    exact (Genlex.Ident "set") >>
    exact (Genlex.Ident "-") >>
    exact (Genlex.Ident "logic") >>
    return (Sym "set-logic")

let check_sat =
  exact (Genlex.Ident "check") >>
  exact (Genlex.Ident "-") >>
  exact (Genlex.Ident "sat") >>
  return (Sym "check-sat")

let atom =
  check_sat <|> set_logic <|> (any >>= (function
    | Genlex.Int i -> return (Int i)
    | Genlex.Ident x -> return (Sym x)
    | _ -> zero
  ))


let parens p =
  satisfy (function Genlex.Kwd "(" -> true | _ -> false)
  >> p <<
  satisfy (function Genlex.Kwd ")" -> true | _ -> false)

let rec parse_sexp i =
  begin
    atom
    <|>
    parens (many parse_sexp => (fun r -> Cons r))
  end i


let of_string s =
  Stream.of_string s |> lex |> many1 parse_sexp

let of_file f =
  open_in f |> Stream.of_channel |> lex |> many1 parse_sexp