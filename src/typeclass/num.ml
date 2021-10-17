module type NUM = sig
  type t

  val zero : t
  val one : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
end

module type INTEGER = sig
include NUM

val of_int : int -> t

val (%) : t -> t -> t
end

module Make(N : NUM) = struct
  include N
  let incr = add one
  let decr = sub one

  let neg = sub zero

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
end

module MakeInt(N: INTEGER) = struct
include N
include Make(N)

let divmod a b =
  let r = a % b in
  let q = (a - r) / b in
  (q, r)

let mid a b =
  let two = of_int 2 in
  let m = a / two + b / two in
  (m, m + (a % two) *  (a % two))
end