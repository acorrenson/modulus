open Lstream
open Logic
open Checker

let rec peek_n (n : int) (l : 'a t) : 'a t =
  lazy begin
    match n, Lazy.force l with
    | 0, _ -> Nil
    | _, Cons (x, xs) -> Cons (x, peek_n (n - 1) xs)
    | _, Nil -> Nil
  end

let pos_int_seq =
  let rec from x = Cons (x, lazy (from (x + 1)))
  in lazy (from 0)


let neg_int_seq =
  let rec from x =
    Cons (x, lazy (from (x - 1)))
  in lazy (from (-1))

let int_seq = interleave pos_int_seq neg_int_seq

let rec combine_list (l : (string * (int t)) list) : ((string * int) list t) =
  match l with
  | [] -> lazy (Cons ([], lazy Nil))
  | (x, vx)::xs ->
    let comb_xs = combine_list xs in
    flat_map (fun n -> map (fun ns -> (x, n)::ns) comb_xs) vx

type filter = int -> bool

let no_filters (l : atom list) =
  List.map (fun x -> x, None) (lvars l)

let model_iterator (max_val : int) (vars : (string * filter option) list) =
  let values = peek_n max_val int_seq in
  List.map (fun (v, f) ->
    match f with
    | Some f -> v, filter f values
    | None -> v, values
  ) vars |> combine_list

let find_model filters n (p : atom list) =
  Option.value filters ~default:(no_filters p)
  |> model_iterator n
  |> find_first (check_list p)
  (* |> Option.fold ~none:Model.UNKNOWN ~some:(fun m -> Model.SAT m) *)
