open Applicative

module type MONAD = sig
include APPLICATIVE

val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Make(M: MONAD) = struct
  include M
  
  let (>>=) = bind
  let (>>) a b = a >>= Fun.const b
  let (let*) = bind
end
