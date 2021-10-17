open Eq

module Cmp = struct
  type t = Lesser | Equal | Higher

  let cmp_fold a b = match a, b with
  | (Equal, b) -> b
  | (Lesser, _) -> Lesser
  | (Higher, _) -> Higher

  let is_lt = (=) Lesser
  let is_le c = c = Lesser || c = Equal
  let is_gt = (=) Higher
  let is_ge c = c = Higher || c = Equal
  let is_eq = (=) Equal
  let is_ne = Fun.negate is_eq
end

module type ORD = sig
  include EQ
  
  val cmp : t -> t -> Cmp.t
end

module Make(O: ORD) = struct
  include O
  
  let lt a b = O.cmp a b |> Cmp.is_lt
  let le a b = O.cmp a b |> Cmp.is_le
  let gt a b = O.cmp a b |> Cmp.is_gt
  let ge a b = O.cmp a b |> Cmp.is_ge

  let min a b = if lt a b then a else b
  let max a b = if gt a b then a else b
end

module OrdEq(O: ORD) = struct
  include Make(O)

  let eq a b = cmp a b |> Cmp.is_eq
end

module StructOrd(T : sig type t end) = Make(struct
  include StructEq(T)
  let cmp (a:t) (b:t) = compare a b |> function
  | 0 -> Cmp.Equal
  | 1 -> Cmp.Higher
  | -1 -> Cmp.Lesser
  | _ -> failwith "Unexpected compare result"
end)