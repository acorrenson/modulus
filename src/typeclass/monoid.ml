module type MONOID = sig
  type t
  val zero : t
  val append : t -> t -> t
end

module Sum(N: Num.NUM): MONOID = struct
  type t = N.t
  
  let zero = N.zero
  let append = N.add
end

module Product(N: Num.NUM): MONOID = struct
  type t = N.t

  let zero = N.one
  let append = N.mul
end

module Or: MONOID = struct
  type t = bool

  let zero = false
  let append = (||)
end

module And: MONOID = struct
  type t = bool

  let zero = true
  let append = (&&)
end
