type 'a lstream =
  | Nil
  | Cons of 'a * ('a lstream ) Lazy.t

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

