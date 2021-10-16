module type SHOW = sig
  type t

  val show : t -> string
end
