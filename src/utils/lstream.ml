
type 'a t = 'a node Lazy.t
and 'a node =
  | Nil
  | Cons of 'a * 'a t

let rec map (f : 'a -> 'b) (s : 'a t) =
  lazy begin
    match Lazy.force s with
    | Nil -> Nil
    | Cons (x, next) -> Cons (f x, map f next)
  end

let copy (s : 'a t) : 'a t = map Fun.id s


let rec interleave (s1 : 'a t) (s2 : 'a t) : 'a t =
  lazy begin
    match Lazy.force s1 with
    | Nil -> Lazy.force s2
    | Cons (x, xs) ->
      Cons (x, interleave s2 xs)
  end

let rec append (l : 'a t) (l' : 'a t) : 'a t =
  lazy begin
    match Lazy.force l with
    | Nil -> Lazy.force l'
    | Cons (x, xs) ->
      Cons (x, append xs l')
  end

let rec flat (l : 'a t t) : 'a t =
  lazy begin
    match Lazy.force l with
    | Nil -> Nil
    | Cons (x, xs) ->
      Lazy.force (append x (flat xs))
  end

let rec flat_map (f : 'a -> 'b t) (l : 'a t) : 'b t =
  lazy begin
    match Lazy.force l with
    | Nil -> Nil
    | Cons (x, xs) ->
      Lazy.force (append (f x) (flat_map f xs))
    end

let rec filter (f : 'a -> bool) (s : 'a t) : 'a t =
  lazy begin
    match Lazy.force s with
    | Cons (x, xs) ->
      if f x then Cons (x, filter f xs)
      else Lazy.force (filter f xs)
    | Nil -> Nil
  end

let rec find_first (f : 'a -> bool) (s : 'a t) : 'a option =
  match Lazy.force s with
  | Nil -> None
  | Cons (x, next) ->
    if f x then Some x
    else find_first f next

let of_stream (s : 'a Stream.t) =
  let rec step s =
    lazy begin
      try Cons (Stream.next s, step s)
      with Stream.Failure -> Nil
    end
  in step s

let of_string (s : string) =
  s |> Stream.of_string |> of_stream

let of_list (l : 'a list) =
  l |> Stream.of_list |> of_stream

let rec to_list (s : 'a t) =
  match Lazy.force s with
  | Cons (x, xs) -> x::to_list xs
  | Nil -> []

let of_channel (i : in_channel) =
  of_stream (Stream.of_channel i)