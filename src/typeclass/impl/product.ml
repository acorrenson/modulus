open Num
open Monoid

module Make(T: NUM): MONOID = struct
  type t = T.t

  let zero = T.one
  let (++) = T.mul
end