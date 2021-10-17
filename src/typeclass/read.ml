module type READ = sig
  type t

  val of_string : string -> t
end