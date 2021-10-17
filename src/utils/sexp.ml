  open Lstream
  open Parsing

  type sexp =
    | Sym of string
    | Int of Bigint.t
    | Cons of sexp list

  let ident =
    spaces >>
      many1 (alpha <|> char '-') => (fun x -> Sym (implode x))
    << spaces

  let operator =
    spaces >> (
      char '+'
      <|> char '-'
      <|> char '/'
      <|> char '*'
      <|> char '='
      <|> char '%'
    ) << spaces => (fun x -> Sym (String.make 1 x))

  let num =
    spaces >>
      number => (fun x -> Int (Bigint.of_string x))
    << spaces

  let atom = ident <|> num <|> operator

  let parens p =
    spaces >> char '(' >> p << char ')' << spaces

  let rec parse_sexp i =
    begin
      atom
      <|>
      parens (many parse_sexp => (fun r -> Cons r))
    end i

  let of_string s =
    Lstream.of_string s |> many1 parse_sexp

  let of_file f =
    open_in f
    |> of_channel
    |> many1 parse_sexp