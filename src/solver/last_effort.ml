open Lstream
open Logic
open Checker

module Batch = struct
  let list_batch (n : int) (l : 'a t) : 'a list =
    let rec step i l k =
      match i, Lazy.force l with
      | 0, _ -> k []
      | _, Cons (x, xs) -> step (i - 1) xs (fun r -> k (x::r))
      | _, Nil -> assert false
    in
    step n l Fun.id
  
  let rec batch (n : int) (l : 'a t) : 'a t =
    lazy begin
      match n, Lazy.force l with
      | 0, _ -> Nil
      | _, Cons (x, xs) -> Cons (x, batch (n - 1) xs)
      | _, Nil -> assert false
    end

  let batch_size = 2

  let pos_int_seq =
    let rec from x = Cons (x, lazy (from (x + 1)))
    in lazy (from 0)


  let neg_int_seq =
    let rec from x =
      Cons (x, lazy (from (x - 1)))
    in lazy (from (-1))

  let int_seq = interleave pos_int_seq neg_int_seq

  let shift (s : 'a t) =
    lazy begin
      match Lazy.force s with
      | Cons (_, xs) -> Lazy.force xs
      | _ -> assert false
    end

  let rec shift_n (n : int) (s : 'a t) =
    if n = 0 then s
    else shift_n (n - 1) (shift s)

  let rec combine_list (l : (string * (int t)) list) : ((string * int) list t) =
    match l with
    | [] -> lazy (Cons ([], lazy Nil))
    | (x, vx)::xs ->
      flat_map (fun n -> map (fun ns -> (x, n)::ns) (combine_list xs)) vx

  let variable_iterators ~batch_size (vars : string list) =
    List.map (fun x -> x, batch batch_size int_seq) vars
  
  let model_iterator ~batch_size ~batch_number (vars : string list) =
    let iterators n = variable_iterators ~batch_size:((n + 1)*batch_size) (List.tl vars) in
    let base n = List.hd vars, batch batch_size (shift_n (n * batch_size) int_seq) in
    List.init batch_number (fun n ->
      combine_list ((base n)::iterators n)
    ) |> of_list |> flat
end

let find_model (p : atom list) =
  let check_one = function
    | None -> false
    | Some m -> check_list m p
  in
  Batch.model_iterator ~batch_size:2 ~batch_number:4 (lvars p)
  |> map (Option.some)
  |> find_first check_one ~default:None
  |> Option.map (fun m -> Model.SAT m)
  |> Option.value ~default:Model.UNKNOWN