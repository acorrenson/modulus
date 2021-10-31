type ('env, 'res) status =
  | Fail of string
  | Abort
  | Update of 'env
  | UpdateValue of 'env * 'res
  | Value of 'res

type ('env, 'res) t = 'env -> ('env, 'res) status

let fail msg = fun _ -> Fail msg

let return v = fun _ -> Value v

let update up = fun env -> Update (up env)

let update_ret v f = fun env -> UpdateValue (f env, v)

let set env = fun _ -> Update env

let abort = fun _ -> Abort

let[@inline] bind m f = fun env ->
  match m env with
  | Value v -> f v env
  | Fail f -> Fail f
  | Abort -> Abort
  | Update e -> Update e
  | UpdateValue (e, v) -> f v e

let[@inline] (let*) m f = bind m f

let[@inline] fast_bind m f = fun env ->
  match m env with
  | Value v -> f v env
  | _ as ret -> ret

let[@inline] (let+) m f = fast_bind m f

let[@inline] (<|>) (m1 : ('env, 'res) t) (m2 : ('env, 'res) t) = fun (env : 'env) ->
  match m1 env with
  | Fail _ | Abort -> m2 env
  | _ as res -> res

let[@inline] (<&>) (s1 : ('env, 'res) t) (s2 : ('env, 'res) t) : ('env, 'res) t = fun env ->
  match s1 env with
  | Update e -> s2 e
  | _ as ret -> ret

let map f m = bind m (fun r -> return (f r))
let[@inline] (=>) = fun m f -> map f m

let fast_map f m = fast_bind m (fun r -> return (f r))
let[@inline] (=>>) = fun m f -> fast_map f m

let ffix (step : ('env, 'res) t -> 'env -> ('env, 'res) t) : ('env, 'res) t =
  let rec go env =
    match step go env env with
    | Update env -> go env
    | _ as ret -> ret
  in go

(* let rec fix s env =
  match s env with
  | Update env -> fix s env
  | _ as ret -> ret *)

let step f = fun env -> f env env

let run f e = f e

let run_opt f e =
  match f e with
  | Value v -> Some v
  | _ -> None

let (<?>) msg m = fun env -> Printf.printf "%s\n" msg; m env