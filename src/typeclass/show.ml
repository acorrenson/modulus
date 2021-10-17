module type SHOW = sig
  type t

  val to_string : t -> string
end
