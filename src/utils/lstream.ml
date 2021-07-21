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

