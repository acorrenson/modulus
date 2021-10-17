module type CATEGORY = sig
  type ('a, 'b) t

  val id : ('a , 'a) t
  val (@@): ('b , 'c) t -> ('c , 'a) t -> ('a , 'c) t
end

module Make(C: CATEGORY) = struct
  include C

  let (<<<) = (@@)
  let (>>>) b a = a @@ b
end