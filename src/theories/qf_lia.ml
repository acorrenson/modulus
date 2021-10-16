(**
  {1 Lia}
  The Modulus internal solver for LIA

  Disclaimer : this module is a "work in progress"
*)

open Logic
open Model

module Interval2 = struct
  type t =
    | Top
    | Bot
    | OpenL of int
    | OpenR of int
    | Intv of int * int

  let singleton v = Intv (v, v)

  let to_string = function
    | Top -> "⊤"
    | Bot -> "⊥"
    | OpenL v -> Printf.sprintf "]-∞; %d]" v
    | OpenR v -> Printf.sprintf "[%d; +∞[" v
    | Intv (lo, hi) -> Printf.sprintf "[%d; %d]" lo hi

  let pp_print fmt x = Format.fprintf fmt "%s" (to_string x)

  let normalize = function
    | Intv (lo, hi) as i -> if lo > hi then Bot else i
    | _ as i -> i

  let add x y =
    let r =
      match x, y with
      | Bot, _ | _, Bot -> Bot
      | Top, _ | _, Top -> Top
      | OpenL v, OpenL v' -> OpenL (v + v')
      | OpenR v, OpenR v' -> OpenR (v + v')
      | OpenL _, OpenR _ | OpenR _, OpenL _ -> Top
      | Intv (_, hi), OpenL v | OpenL v, Intv (_, hi) -> OpenL (hi + v)
      | Intv (lo, _), OpenR v | OpenR v, Intv (lo, _) -> OpenR (lo + v)
      | Intv (lo, hi), Intv (lo', hi') -> Intv (lo + lo', hi + hi')
    in normalize r

  let inter x y =
    let r =
      match x, y with
      | Bot, _ | _, Bot -> Bot
      | Top, x | x, Top -> x
      | OpenL v, OpenL v' -> OpenL (min v v')
      | OpenR v, OpenR v' -> OpenR (max v v')
      | OpenL v, OpenR v' | OpenR v', OpenL v -> Intv (min v v', max v v')
      | Intv (lo, hi), Intv (lo', hi') -> Intv (max lo lo', min hi hi')
      | OpenL v, Intv (lo, _) | Intv (lo, _), OpenL v -> OpenL (min v lo)
      | OpenR v, Intv (_, hi) | Intv (_, hi), OpenR v -> OpenR (max v hi)
    in normalize r

  let is_empty i = (i = Bot)

  let neg = function
    | Bot -> Bot
    | Top -> Top
    | OpenL v -> OpenR (-v)
    | OpenR v -> OpenL (-v)
    | Intv (lo, hi) -> Intv (-hi, -lo)
  
  let sub x y = add x (neg y)

  let add_inv x y r = (sub r y, sub r x)

  let _mid x y =
    let m = x / 2 + y / 2 in
    (m, m + (x land 1) * (y land 1))

  let split = function
    | Top -> `Split (OpenL 0, OpenR 0)
    | Bot -> failwith "cannot split ⊥"
    | Intv (lo, hi) ->
      if lo = hi then `Single lo else
      let (m, m') = _mid lo hi in
      `Split (Intv (lo, m), Intv (m', hi))
    | OpenL v ->
      `Split (OpenL (v - 1), Intv (v, v))
    | OpenR v ->
      `Split (Intv (v, v), OpenR (v + 1))

  let peek = function
      | Top -> 0
      | Bot -> failwith "cannot peek a value in ⊥"
      | Intv (lo, _) -> lo
      | OpenL v | OpenR v -> v
end

(* module Interval = struct
  type t = { lo : int; hi : int }

  let sum_overflow x y =
    if y > 0 then x + y <= x
    else x + y > x

  let _add_int x y =
    if sum_overflow x y then
      if x > 0 then max_int
      else min_int
    else x + y

  let add (i1 : t) (i2 : t) : t =
    { lo = _add_int i1.lo i2.lo; hi = _add_int i1.hi i2.hi }

  let sub (i1 : t) (i2 : t) : t =
    { lo = _add_int i1.lo (-i2.hi); hi = _add_int i1.hi (-i2.lo) }

  let add_inv (i1 : t) (i2 : t) (res : t) = (sub res i2, sub res i1)
  
  let inter (i1 : t) (i2 : t) : t =
    { lo = max i1.lo i2.lo; hi = min i1.hi i2.hi }
  
  let is_empty ({ lo; hi }) : bool = lo > hi

  let top : t = { lo = min_int; hi = max_int }

  let singleton (x : int) : t = { lo = x; hi = x}

  let mid x y =
    let m = x / 2 + y / 2 in
    (m, m + (x land 1) * (y land 1))

  let split ({lo; hi} : t) =
    Printf.printf "split on [%d %d]\n" lo hi;
    if lo + 1 = hi then
      `Pair (singleton lo, singleton hi)
    else if lo = hi then
      `Single lo
    else
      let (m, m') = mid lo hi in
      `Pair ({ lo; hi = m }, { lo = m'; hi })
end *)

module Solver = struct
  type state = (term * Interval2.t) list

  type 'a status =
    | Value of 'a
    | Update of state * 'a
    | Fail of string

  type 'a update = state -> 'a status

  let return (x : 'a) : 'a update = fun _ -> Value x

  let update t x : 'a update = fun s ->
    let open Interval2 in
    match List.assoc_opt t s with
    | Some x' ->
      if x = x' then Value x'
      else
        let d = inter x x' in
        if is_empty d then
          Fail (Printf.sprintf "update failed %s ∩ %s = %s"
            (to_string x) (to_string x') (to_string d))
        else Update ((t, d)::List.remove_assoc t s, d)
    | None ->
      Update ((t, x)::s, x)

  exception UpdateFail of string

  let get (s : 'a status) : 'a =
    match s with
    | Fail l -> raise (UpdateFail ("nothing to get because: " ^ l))
    | Value v -> v
    | Update (_, v) -> v

  let (let*) (m : 'a update) (f : 'a -> 'b update) : 'b update = fun s ->
    match m s with
    | Fail _ as err -> err
    | Value v -> f v s
    | Update (e, v) ->
      match f v e with
      | Value v -> Update (e, v)
      | _ as ret -> ret

  let rec eval (x : term) : Interval2.t update = fun s ->
    match List.assoc_opt x s with
    | Some v -> Value v
    | None ->
      match x with
      | Var _ -> update x Interval2.Top s
      | Cst v -> update x (Interval2.singleton v) s
      | Add (t1, t2) ->
        begin
          let* v1 = eval t1 in
          let* v2 = eval t2 in
          update x (Interval2.add v1 v2)
        end s

  exception Contradiction of string

  let (>>) (u1 : 'a update) (u2 : 'b update) : 'b update =
    let* _ = u1 in u2

  let leak : state update = fun s -> Value s

  let print_state =
    let open Interval2 in
    List.iter (fun (x, d) ->
      match x with
      | Var x ->
        Format.printf "%s := %a\n" x pp_print d
      | _ -> ()
    )

  let fail msg = fun _ -> Fail msg

  let propagate_one (Eq (t1, t2) : atom) : unit update =
    let* d1 = eval t1 in
    let* d2 = eval t2 in
    let d = Interval2.inter d1 d2 in
    update t1 d >> update t2 d >> return ()

  let propagate_one_backward (Eq (t1, t2) : atom) : unit update =
    let rec step (t : term) (dt : Interval2.t) : unit update =
      match t with
      | Cst _ | Var _ -> update t dt >> return ()
      | Add (t1, t2) ->
        let* d1 = eval t1 in
        let* d2 = eval t2 in
        let (d1', d2') = Interval2.add_inv d1 d2 dt in
        step t1 d1' >> step t2 d2' >> return ()
    in
    let* d1 = eval t1 in
    let* d2 = eval t2 in
    step t1 d1 >> step t2 d2


  let sequence (l : 'a list) (p : 'a -> unit update) : unit update =
    List.fold_left (>>) (return ()) (List.map p l)

  let propagate l : unit update =
    sequence l propagate_one
    >> sequence l propagate_one_backward

  let (<|>) (u1 : 'a update) (u2 : 'b update) : 'b update = fun s ->
    match u1 s with
    | Fail _ ->
      Printf.printf "fallback...\n";
      u2 s
    | _ as r -> r


  (* let last_effort (p : atom list) =
    let vars =
      List.map avars p
      |> List.fold_left VSet.union VSet.empty
      |> VSet.to_seq
      |> List.of_seq
    in
    let rec step  *)


  let extract_model (p : atom list) =
    let vars =
        List.map avars p
        |> List.fold_left VSet.union VSet.empty
        |> VSet.to_seq
        |> List.of_seq
    in
    let rec step vlist (model : Model.t) : Model.t update =
      match vlist with
      | [] ->
        if List.for_all (fun a -> Option.get (check_atom a model)) p
        then return model
        else fail "extract model"
      | x::xs ->
        let* dx = eval (Var x) in
        match Interval2.split dx with
        | `Split (d1, d2) ->
          let open Interval2 in
          let c1, c2 = peek d1, peek d2 in
          (update (Var x) (singleton c1) >> propagate p >> (step xs ((x, c1)::model)))
          <|>
          (update (Var x) (singleton c2) >> propagate p >> (step xs ((x, c2)::model)))
          <|>
          (update (Var x) d1 >> propagate p >> step vlist model)
          <|>
          (update (Var x) d2 >> propagate p >> step vlist model)
        | `Single v ->
          step xs ((x, v)::model)
      in
      step vars []
  
  let solve (p : atom list) =
    let go = propagate p >> extract_model p in
    get (go [])
end

(**
  Solve a single arithmetic goal
*)
let lia1 (a : atom) : anwser =
  match a with
  | Eq (Var x, Var y) -> SAT [x, 0; y, 0]
  | Eq (Cst x, Cst y) ->
    if x = y then SAT [] else UNSAT
  | Eq (Var x, Cst a)
  | Eq (Cst a, Var x) -> SAT [x, a]
  | _ -> UNKNOWN

let debug m x = match m x with Some x -> string_of_int x | _ -> "?"

(**
  Solve a list of arithmetic goals under the hypothesis
  that goals are independants (don't share variables)
*)
let rec lia_indep (l : atom list) : anwser =
  match l with
  | [] -> SAT []
  | eq::eqs ->
    match lia1 eq with
    | SAT m ->
      begin match lia_indep eqs with
      | SAT m' -> SAT (m @ m')
      | _ as r -> r
      end
    | UNSAT -> UNSAT
    | UNKNOWN ->
      match lia_indep eqs with
      | SAT _ -> UNKNOWN
      | _ as r -> r

let lia (l : atom list) : anwser =
  try SAT (Solver.solve l)
  with
  | Solver.UpdateFail _msg ->
    Printf.printf "unsat because : %s\n" _msg;
    UNSAT
  | _ -> UNKNOWN

let is_sat = function SAT _ -> true | _ -> false

