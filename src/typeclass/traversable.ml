open Applicative
open Functor
open Foldable

module Make(A: APPLICATIVE) = struct
  module type TRAVERSABLE = sig
    include FUNCTOR
    include FOLDABLE with type 'a t := 'a t

    val traverse : ('a -> 'b A.t) -> 'a t -> 'b t A.t
  end

  module Extend(T: TRAVERSABLE) = struct
    include T

    let sequence s = traverse Fun.id s
  end
end

module Monad(M: Monad.MONAD) = struct
  module TM = Make(M)
  module MM = Monad.Make(M)
  module Extend(T: TM.TRAVERSABLE) = struct
    include TM.Extend(T)

    let map_monad action l =
      let k a r =
        let open MM in
        let* x = action a in
        let* xs = r in
        pure (x::xs)
      in fold_right k (MM.pure []) l

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
end