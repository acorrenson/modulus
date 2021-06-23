open Sat

type term =
  | Var of int

type atom =
  | Eq of term * term

type formula =
  | Or of formula * formula
  | And of formula * formula
  | Neg of formula
  | Atom of atom

module Cnf = struct
  type t =
    | Or of t * t
    | And of t * t
    | Lit of bool * atom
end

let rec cnf_pass_neg (s : bool) (form : formula) =
  match form with
  | Or (f1, f2) ->
    if s then
      Cnf.Or (cnf_pass_neg true f1, cnf_pass_neg true f2)
    else
      Cnf.And (cnf_pass_neg false f1, cnf_pass_neg false f2)
  | And (f1, f2) ->
    if s then
      And (cnf_pass_neg true f1, cnf_pass_neg true f2)
    else
      Or (cnf_pass_neg false f1, cnf_pass_neg false f2)
  | Neg f ->
    cnf_pass_neg (not s) f
  | Atom a -> Lit (s, a)

let rec merge_clause (cl1 : clause) (cl2 : clause) =
  match cl1 with
  | x::xs ->
    if List.mem x cl2 then merge_clause xs cl2
    else x::(merge_clause xs cl2)
  | [] -> cl2

let merge_clause_cnf (cl : clause) (cn : cnf) : cnf =
  List.map (merge_clause cl) cn

let rec merge_cnf (cn1 : cnf) (cn2 : cnf) : cnf =
  match cn1 with
  | [] -> []
  | c::cs -> (merge_clause_cnf c cn2) @ (merge_cnf cs cn2)

type vmap = (int, atom) Hashtbl.t

let to_cnf (form : formula) : cnf * vmap =
  let h = Hashtbl.create 10 in
  let i = ref 0 in
  let rec to_cnf_aux (form : Cnf.t) =
    match form with
    | And (f1, f2) ->
      (to_cnf_aux f1) @ (to_cnf_aux f2)
    | Or (f1, f2) ->
      merge_cnf (to_cnf_aux f1) (to_cnf_aux f2)
    | Lit (s, a) ->
      incr i; Hashtbl.add h !i a;
      if s then [[!i]] else [[-(!i)]]
  in
  to_cnf_aux (cnf_pass_neg true form), h