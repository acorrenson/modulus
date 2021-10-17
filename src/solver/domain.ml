(**
  This type represents a partition of a research space of type ['domain].
  [Split (a, b)] modelize a partition of the space in 2 parts.
  [Single v] is used when the space to be partitioned is 
  already restricted to a single value of type ['value]
*)
type ('domain, 'value) choice =
  | Split of 'domain * 'domain
  | Single of 'value

exception NoChoice

module type Domain = sig
  type t
  val top : t
  val bot : t
  val inter : t -> t -> t
  val singleton : int -> t
  val is_empty : t -> bool
  val pp_print : Format.formatter -> t -> unit
  val add : t -> t -> t
  val add_inv : t -> t -> t -> (t * t)
  val split : t -> (t , int) choice
  val peek : t -> int
end

module Combine (A : Domain) (B : Domain) : Domain = struct

type t =
  | Bot
  | Pair of (A.t * B.t)

let normalize x y =
  if x = A.bot || y = B.bot then Bot
  else Pair (x, y)

let bot = Bot
let top = Pair (A.top, B.top)

let singleton v = Pair (A.singleton v, B.singleton v)

let split = function
  | Bot -> raise NoChoice
  | Pair (x, y) ->
    match A.split x with
    | Split (x1, x2) ->
      Split (normalize x1 y, normalize x2 y)
    | Single v ->
      match B.split y with
      | Split (y1, y2) ->
        Split (normalize (A.singleton v) y1, normalize (A.singleton v) y2)
      | Single v' ->
        if v = v' then Single v
        else raise NoChoice

let peek = function
  | Bot -> raise NoChoice
  | Pair (x, _) ->
    A.peek x

let inter x y =
  match x, y with
  | Bot, _ | _, Bot -> Bot
  | Pair (x, y), Pair (x', y') ->
    normalize (A.inter x x') (B.inter y y')

let add x y =
  match x, y with
  | Bot, _ | _, Bot -> Bot
  | Pair (x, y), Pair (x', y') ->
    normalize (A.add x x') (B.add y y')

let add_inv x y z =
  match x, y, z with
  | Bot, _, _ | _, Bot, _ | _, _, Bot -> Bot, Bot
  | Pair (x, y), Pair (x', y'), Pair (r1, r2) ->
    let (x1, x2) = A.add_inv x x' r1 in
    let (y1, y2) = B.add_inv y y' r2 in
    normalize x1 y1, normalize x2 y2

let is_empty = function Bot -> true | _ -> false

let pp_print fmt = function
  | Bot -> Format.fprintf fmt "⊥"
  | Pair (x, y) ->
    Format.fprintf fmt "%a × %a" A.pp_print x B.pp_print y
end