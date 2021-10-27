type clause = int list

type cnf = clause list

let string_of_clause c =
  "{" ^ String.concat " " (List.map string_of_int c) ^ "}"

let string_of_cnf c =
  "{" ^ String.concat " " (List.map string_of_clause c) ^ "}"

module SolverMonad : sig

type ('env, 'res) status

type ('env, 'res) t

val return : 'res -> ('env, 'res) t

val fail : string -> ('env, 'res) t

val abort : ('env, 'res) t

val update : ('env -> 'env) -> ('env, 'res) t

val set : 'env -> ('env, 'res) t

val bind : ('env, 'res) t -> ('res -> ('env, 'res2) t) -> ('env, 'res2) t
val (let*) : ('env, 'res) t -> ('res -> ('env, 'res2) t) -> ('env, 'res2) t

val fast_bind : ('env, 'res) t -> ('res -> ('env, 'res) t) -> ('env, 'res) t
val (let+) : ('env, 'res) t -> ('res -> ('env, 'res) t) -> ('env, 'res) t

val (<|>) : ('env, 'res) t -> ('env, 'res) t -> ('env, 'res) t

val (<&>) : ('env, 'res) t -> ('env, 'res) t -> ('env, 'res) t

val map : ('res -> 'res1) -> ('env, 'res) t -> ('env, 'res1) t
val (=>) : ('env, 'res) t -> ('res -> 'res1) -> ('env, 'res1) t

val fast_map : ('res -> 'res) -> ('env, 'res) t -> ('env, 'res) t
val (=>>) : ('env, 'res) t -> ('res -> 'res) -> ('env, 'res) t

val strategy : (('env, 'res) t -> 'env -> ('env, 'res) t) -> ('env, 'res) t

val run : ('env, 'res) t -> 'env -> 'res option

end = struct

type ('env, 'res) status =
  | Fail of string
  | Abort
  | Update of 'env
  | Value of 'res

type ('env, 'res) t = 'env -> ('env, 'res) status

let fail msg = fun _ -> Fail msg

let return v = fun _ -> Value v

let update up = fun env -> Update (up env)

let set env = fun _ -> Update env

let abort = fun _ -> Abort

let[@inline] bind m f = fun env ->
  match m env with
  | Value v -> f v env
  | Fail f -> Fail f
  | Abort -> Abort
  | Update e -> Update e

let[@inline] (let*) m f = bind m f

let[@inline] fast_bind m f = fun env ->
  match m env with
  | Value v -> f v env
  | _ as ret -> ret

let[@inline] (let+) m f = fast_bind m f

let[@inline] (<|>) (m1 : ('env, 'res) t) (m2 : ('env, 'res) t) = fun (env : 'env) ->
  match m1 env with
  | Fail _ -> m2 env
  | _ as res -> res

let[@inline] (<&>) (s1 : ('env, 'res) t) (s2 : ('env, 'res) t) : ('env, 'res) t = fun env ->
  match s1 env with
  | Update e -> s2 e
  | _ as ret -> ret

let map f m = bind m (fun r -> return (f r))
let[@inline] (=>) = fun m f -> map f m

let fast_map f m = fast_bind m (fun r -> return (f r))
let[@inline] (=>>) = fun m f -> fast_map f m

let strategy (step : ('env, 'res) t -> 'env -> ('env, 'res) t) : ('env, 'res) t =
  let rec go env =
    match step go env env with
    | Update env -> go env
    | _ as ret -> ret
  in go

let run f e = match f e with Value v -> Some v | _ -> None
end

let remove_lit (l : int) = List.filter ((<>) l)

open SolverMonad

let propagate l =
  List.filter_map (fun c ->
    if List.mem l c then None
    else Some (remove_lit (-l) c)
  ) |> SolverMonad.update

let add l = fun ls -> l::ls

type ('a, 'b) solver = ('a, 'b) t

let solve : (cnf, int list) solver = strategy (fun next -> function
  | [] -> return []
  | []::_ -> fail "unsat"
  | (l::_)::_ -> 
    (propagate l <&> next =>> add l)
    <|>
    (propagate (-l) <&> next =>> add (-l))
)