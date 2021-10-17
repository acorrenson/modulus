open Semigroup

module type MONOID = sig
  include SEMIGROUP
  val zero : t
end