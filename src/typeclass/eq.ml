module type EQ = sig
  type t
  val eq : t -> t -> bool
end

module Eq(E: EQ) = struct
  include E
  let ne l = Fun.negate (eq l)

  let (==) = eq
  let (!=) = ne
end