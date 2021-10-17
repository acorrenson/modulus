module type ALTERNATIVE = sig
  include Applicative.APPLICATIVE

  val empty : 'a t

  val (<|>) : 'a t -> 'a t -> 'a t
end

module Make(A: ALTERNATIVE) = struct
  include A
  module AA = Applicative.Make(A)

  let rec some v = AA.lift_a2 List.cons v (many v)
  and many v = some v <|> pure []
end