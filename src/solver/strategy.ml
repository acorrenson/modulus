type ('env, 'res) status =
  | Abort of string
  | Contradict
  | Update of 'env
  | UpdateValue of 'env * 'res
  | Value of 'res

type ('env, 'res) t = 'env -> ('env, 'res) status

let abort msg = fun _ -> Abort msg

let return v = fun _ -> Value v

let contradict = fun _ -> Contradict

let update up = fun env -> Update (up env)

let skip = fun env -> Update env

let update_ret v f = fun env -> UpdateValue (f env, v)

let set env = fun _ -> Update env

let[@inline] bind m f = fun env ->
  match m env with
  | Contradict -> Contradict
  | Value v -> f v env
  | Abort f -> Abort f
  | Update e -> Update e
  | UpdateValue (e, v) -> f v e

let[@inline] (let*) m f = bind m f

let[@inline] fast_bind m f = fun env ->
  match m env with
  | Value v -> f v env
  | UpdateValue (e, v) -> f v e
  | _ as ret -> ret

let[@inline] (let+) m f = fast_bind m f

let[@inline] (<|>) (m1 : ('env, 'res) t) (m2 : ('env, 'res) t) = fun (env : 'env) ->
  match m1 env with
  | Contradict | Abort _ -> m2 env
  | _ as res -> res

let[@inline] (<&>) (s1 : ('env, 'res) t) (s2 : ('env, 'res) t) : ('env, 'res) t = fun env ->
  match s1 env with
  | Update e -> s2 e
  | Abort _ -> s2 env
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

let rec stabilize s env =
  match s env with
  | Update env -> stabilize s env
  | _ as ret -> ret

let step f = fun env -> f env env

let run f e = f e

let run_opt f e =
  match f e with
  | Value v -> Some v
  | _ -> None

let (<?>) msg m = fun env -> Printf.printf "%s\n" msg; m env