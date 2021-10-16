module type FUNCTOR = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Functor(F: FUNCTOR) = struct
  include F

  let (<$>) = map
end
