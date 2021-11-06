type ('env, 'res) status =
  | Fail of string
  | Abort
  | Update of 'env
  | UpdateValue of 'env * 'res
  | Value of 'res

type ('env, 'res) t = 'env -> ('env, 'res) status

let[@inline] coerce_status : (('env, 'a) status -> ('env, 'b) status) =
  function
  | Value _ | UpdateValue _ -> assert false
  | Fail _ as x -> x
  | Abort as x -> x
  | Update _ as x -> x

let update up = fun env -> Update (up env)

let update_ret v f = fun env -> UpdateValue (f env, v)

let set env = fun _ -> Update env

let abort = fun _ -> Abort

let run f e = f e

let run_opt f e =
  match f e with
  | Value v -> Some v
  | _ -> None

module Make(Env: sig type t end) : sig
  include Minicat.Monad.MONAD with type 'a t = (Env.t -> (Env.t, 'a) status)
  include Minicat.Alternative.ALTERNATIVE with type 'a t := 'a t

  val ffix : ('a t -> Env.t -> 'a t) -> 'a t

  val step : (Env.t -> 'a t) -> 'a t

  val (<&>) : 'a t -> 'a t -> 'a t

  val (<?>) : string -> 'a t -> 'a t
end = struct

  type 'res t = Env.t -> (Env.t, 'res) status

  let empty = fun _ -> Abort

  let fail msg = fun _ -> Fail msg

  let pure v = fun _ -> Value v

  let[@inline] bind m f = fun env ->
    match m env with
    | Value v -> f v env
    | UpdateValue (e, v) -> f v e
    | x -> coerce_status x

  let[@inline] alt m1 m2 = fun (env : Env.t) ->
    match m1 env with
    | Fail _ | Abort -> m2 env
    | _ as res -> res

  let[@inline] (<&>) s1 s2 = fun env ->
    match s1 env with
    | Update e -> s2 e
    | _ as ret -> ret

  let map f m = bind m (fun r -> pure (f r))

  let app fm xm = bind fm (fun f -> map f xm)

  let ffix (step : 'a t -> Env.t -> 'a t) : 'a t =
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

  let (<?>) msg m = fun env -> Printf.printf "%s\n" msg; m env
end