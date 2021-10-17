type 'a lstream =
  | Nil
  | Cons of 'a * ('a lstream ) Lazy.t

let rec map f s =
  match s with
  | Nil -> Nil
  | Cons (x, next) -> Cons (f x, lazy (map f (Lazy.force next)))

let rec find_first f ~default s =
  match s with
  | Nil -> default
  | Cons (x, next) ->
    if f x then x
    else find_first f ~default (Lazy.force next)

let of_stream (s : 'a Stream.t) =
  let rec step s =
    try Cons(Stream.next s, lazy (step s))
    with Stream.Failure -> Nil
  in
  step s

let of_string (s : string) =
  s |> Stream.of_string |> of_stream

let of_list (l : 'a list) =
  l |> Stream.of_list |> of_stream

let of_channel (i : in_channel) =
  of_stream (Stream.of_channel i)

let first = function
| Nil -> failwith "No value in lazy stream"
| Cons (x, _) -> x

let empty = function
| Nil -> true
| Cons _ -> false

let last =
  let rec aux acc = function
  | Nil -> acc
  | Cons (x, xs) -> aux x (Lazy.force xs)
in function
| Nil -> failwith "No value in lazy stream"
| Cons (x, xs) -> lazy (aux x (Lazy.force xs))

let rec zip a b = match a,b with
| (Nil, _) | (_, Nil) -> Nil
| (Cons (x,xs), Cons(y,ys)) -> Cons ((x,y), lazy (zip (Lazy.force xs) (Lazy.force ys)))

let rec scan_left f i = function
| Nil -> Nil
| Cons (x,xs) -> let r = f i x in Cons (r, lazy (scan_left f r (Lazy.force xs)))

let fold_left f i l = scan_left f i l |> last 

let rec take_while pred = function
| Nil -> Nil
| Cons (x,xs) -> if pred x then Nil else Cons (x, lazy (take_while pred (Lazy.force xs)))

let rec skip_while pred = function
| Nil -> Nil
| Cons (x, xs) -> if pred x
  then skip_while pred (Lazy.force xs)
  else Cons (x, xs)