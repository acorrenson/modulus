module type NUM = sig
  type t

  val zero : t
  val one : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
end

module Make(N : NUM) = struct
  include N
  let incr = add one
  let decr = sub one

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
end