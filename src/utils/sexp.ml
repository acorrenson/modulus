open Minilib.Parsing
open Minicat.Monad.Make(Minilib.Parsing)
open Minicat.Alternative.Make(Minilib.Parsing)

type sexp =
  | Sym of string
  | Int of int
  | Cons of sexp list

let ident =
  let open Fold(Minicat_ext.Seq_ext) in (* < provides [many1] over [Seq.t] *)
  let+ sym = token (many1 (alpha <|> char '-')) in
  Sym (String.of_seq sym)

let operator =
  let op' = ['+';'-';'/';'*';'=';'%'] |> List.map char |> choice in
  let+ op = token op' in
  Sym (String.make 1 op)
let num =
  let open Fold(Minicat_ext.Seq_ext) in
  let+ chars = many1 digit in
  let num = String.of_seq chars |> int_of_string in 
  Int num

let atom = ident <|> num <|> operator <?> "atom"

let parens p = surround (token (char '(')) (token (char ')')) p <?> "parentheses"

let sexp =
  let call sexp = let+ vals = many sexp in Cons vals in
  fix (fun sexp -> parens (call sexp) <|> atom <?> "fix") <?> "sexp"

let parser = many sexp <* eof

let of_string s = parse_string parser s

let of_file f = parse_file parser f
