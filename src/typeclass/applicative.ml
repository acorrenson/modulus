open Functor
module type APPLICATIVE = sig
  include FUNCTOR

  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module Make(A: APPLICATIVE) = struct
  include A
  include Functor.Make(A)

  let (<*>) = apply
  let lift_a2 f x = apply (f <$> x)
  let ( *> ) a b = (Fun.id <$ a) <*> b 
  let ( <* ) a = lift_a2 Fun.const a
end
