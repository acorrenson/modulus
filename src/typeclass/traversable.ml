open Applicative
open Functor
open Foldable

module type TRAVERSABLE = functor (A: APPLICATIVE) -> sig
  include FUNCTOR
  include FOLDABLE with type 'a t := 'a t

  val traverse : ('a -> 'b A.t) -> 'a t -> 'b t A.t
end

module Make(T: TRAVERSABLE)(A: APPLICATIVE) = struct
  include T(A)

  let sequence s = traverse Fun.id s
end

module Monad(T: TRAVERSABLE)(M: Monad.MONAD) = struct
  include Make(T)(M)
  module MM = Monad.Make(M)

  let map_monad action l =
    let open MM in
    let k a r =
      let* x = action a in
      let* xs = r in
      pure (x::xs)
    in fold_right k (pure []) l

  let sequence l = map_monad Fun.id l
  let lift_monad f m =
    let open MM in
    let* x = m in pure (f x)
  let lift_m2 f m n =
    let open MM in
    let* x = m in
    let* y = n in
    pure (f x y)
end