type ('a, 'b) t = 'a -> 'b

let id = Fun.id
let arr = Fun.id

let ( *** ) f g (x,y) = (f x, g y)

module MakeApplicative(A: sig type t end): Applicative.APPLICATIVE = struct
  type 'a t = A.t -> 'a

  let map fab fra r = fab (fra r)
  let pure = Fun.const
  let apply frab fra r = let a = fra r in frab r a
end
