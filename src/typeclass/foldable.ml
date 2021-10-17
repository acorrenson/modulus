open Monoid

module type FOLDABLE = sig
  type 'a t

  val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
end

module FoldMonoid(F: FOLDABLE)(M: MONOID) = struct
  type t = M.t F.t
  open M

  let fold_map f = F.fold_right (fun a -> (++) (f a)) zero
end

module Make(F: FOLDABLE) = struct
  include F

  (* let fold_left f i l = failwith "TODO" *)

  let null l = fold_right (fun _ _ -> false) true l
  let first l = fold_right (fun a -> function | None -> Some a | Some v -> Some v) None l
  let length l = fold_right ( fun _ -> (+) 1) 0 l

  let any pred l = fold_right (fun a -> (||) (pred a)) false l
  let all pred l = fold_right (fun a -> (&&) (pred a)) true l
end

module FoldNum(F: FOLDABLE)(N: Num.NUM) = struct
  type t = N.t F.t

  let sum = F.fold_right N.add N.zero
  let product = F.fold_right N.mul N.one
end

module FoldEq(F: FOLDABLE)(E: Eq.EQ) = struct
  type t = E.t F.t
  module FF = Make(F)

  let elem el = FF.any (E.equal el)
  let not_elem el = Fun.negate (elem el)
end

module FoldOrd(F: FOLDABLE)(O: Ord.ORD) = struct
  type t = O.t F.t
  module OO = Ord.Make(O)
  module FF = Make(F)

  let min l =
    match FF.first l with
    | None -> None
    | Some v -> Some (F.fold_right OO.min v l)

  let max l =
    match FF.first l with
    | None -> None
    | Some v -> Some (F.fold_right OO.max v l)
end
