module type FUNCTOR = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Make(F: FUNCTOR) = struct
  include F

  let (<$>) = map
  let (<$) a = map (Fun.const a)
end
