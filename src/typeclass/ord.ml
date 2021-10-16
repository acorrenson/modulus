open Eq

module type ORD = sig
  include EQ
  val lt : t -> t -> bool
  val le : t -> t -> bool
end

module Ord(O: ORD) = struct
  include O
  let gt r l = le l r
  let ge r l = lt l r
end