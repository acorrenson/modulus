open Monoid

module Make(T: sig type t end): MONOID = struct
  type t = T.t -> T.t

  let zero = Fun.id
  let (++) f g x = f (g x)
end