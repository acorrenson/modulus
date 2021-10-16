open Applicative

module type MONAD = sig
include APPLICATIVE

val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Monad(M: MONAD) = struct
  include M
  let (>>=) = bind
  let (let*) = bind
end
