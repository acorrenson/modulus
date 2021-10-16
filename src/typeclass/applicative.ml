open Functor
module type APPLICATIVE = sig
  include FUNCTOR

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module Applicative(A: APPLICATIVE) = struct
  include A

  let (<*>) = apply
end
