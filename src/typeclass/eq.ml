module type EQ = sig
  type t
  val equal : t -> t -> bool
end

module Make(E: EQ) = struct
  include E
  let ne l = Fun.negate (equal l)

  let (==) = equal
  let (!=) = ne
end

module StructEq(T: sig type t end) = Make(struct
  include T
  let equal = (=)
end)