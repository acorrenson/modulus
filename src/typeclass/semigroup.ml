module type SEMIGROUP = sig
  type t

  val (<>) : t -> t -> t
end