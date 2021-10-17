open Lstream

type ('i, 'o) presult = ('o * 'i lstream) option

type ('i, 'o) parser = 'i lstream -> ('i, 'o) presult

let zero _ = None

let return x = fun i -> Some (x, i)

let (>>=) p fp = fun i ->
  match p i with
  | None -> None
  | Some (o, i') -> fp o i'

let any : ('a, 'a) parser = function
  | Nil -> None
  | Cons (x, i) -> Some (x, Lazy.force i)

let satisfy test =
  any >>= (fun res -> if test res then return res else zero)

let eof x = function
  | Nil -> Some (x, Nil)
  | Cons _ -> None

let (=>) x f = x >>= fun r -> return (f r)

let (>>) x y = x >>= fun _ -> y

let (<<) x y = x >>= fun r -> y >>= fun _ -> return r

let (<~>) x xs = x >>= fun r -> xs >>= fun rs -> return (r :: rs)

let (<|>) p p' = fun i ->
  match p i with
  | None -> p' i
  | Some _ as r -> r

let option default x = x <|> return default

let rec many p = option [] (p >>= fun r -> many p >>= fun rs -> return (r :: rs))

let many1 p = p <~> many p

let char (c : char) = satisfy ((=) c)

let digit = satisfy (function '0'..'9' -> true | _ -> false)

let implode x = String.concat "" @@ List.map (String.make 1) x

let number = many1 digit => implode

let alpha =
  satisfy (function 'a'..'z' | 'A'..'Z' -> true | _ -> false)

let spaces =
  many (satisfy (function '\n' | '\t' | ' ' -> true | _ -> false))

let (let*) = (>>=)