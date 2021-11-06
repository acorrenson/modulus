(**
  This module provides utilities to define new resolution strategies.
  We introduce a monad [('env, 'res) Strategy.t] to build customized solvers.

  Intuitively, a value of type [('env, 'res) Strategy.t] is a function from ['env -> 'res]
  where ['env] is the type of resolution contexts and ['res] is the result of the strategy.

  A strategy may or may not return a valid result. To modelize this situation, the stragegies results are
  wrapped in the the [('env, 'res) status] type in order to give additional
  information on strategies executions.

  The call [run s e] executes the strategy [s] in context [e] and return a status information.
*)

type ('env, 'res) status =
    Fail of string
  | Abort
  | Update of 'env
  | UpdateValue of 'env * 'res
  | Value of 'res
(**
  Status message reported by strategies.
  The status is either:
  {ul
    {- A failure with a message }
    {- A value indicating that the strategy successfully computed a result }
    {- An update notification indicating that the strategy is not done }
    {- An update notification together with an intermediate value.
      This indicate that the strategy computed an intermediate value [v]
      and updated the environment. It is especially useful to compose intermediate steps.
    }
    {- A forced interruption of the strategy }
  }
*)

type ('env, 'a) t

val update : ('env -> 'env) -> ('env, 'a) t
(** [update f] updates the environment of the strategy monad by applying [f] on it, and saving the result. *)

val update_ret : 'a -> ('env -> 'env) -> ('env, 'a) t
(** [update_ret v f] updates the environment like [update f], but also combines it wdith a [pure v] as the return value. *)

val set : 'env -> ('env, 'a) t
(** [set env] replaces the current environment by the one provided in [env]. It is the equivalent of [update (Fun.const env)]. *)

val abort : ('env, 'a) t
(** Aborts computation immediately. *)


val run : ('env, 'a) t -> 'env -> ('env, 'a) status
(**
  [run s e] executes strategy [s] in environnement [e]
*)

val run_opt : ('env, 'res) t -> 'env -> 'res option
(**
  [run_opt s e] is similar to [run s e] but convert the resulting [status]
  message to an optional value. If [s] returns only a value [v] in context [e], [run_opt s e] is [Some v]
  otherwise, [run_opt s e] is [None].
*)

(** Creates the strategy monad, passing in an instance of the environment type. *)
module Make :
  functor (Env : sig type t end) ->
    sig
      include Minicat.Monad.MONAD with type 'a t = (Env.t, 'a) t
      include Minicat.Alternative.ALTERNATIVE with type 'a t := 'a t

      val ffix : ('a t -> Env.t -> 'a t) -> 'a t
      (**
        [ffix] computes the fixpoint of a parametrized strategy.
        If [step] is a function computing a strategy given a strategy [recall] and a context [env],
        [fix step] computes a recursive strategy which is similar to [step] but where
        every call to the strategy [recall] are recursive calls to the strategy [step] itself.
        The recursion continues while [step] returns an update notification.
        It may not terminates !
      *)

      val step : (Env.t -> 'a t) -> 'a t
      (**
        [step] is used to choose which strategy to apply given a current environment.
        If [f] is a function from ['env] to [('env, 'res) t] (that is, a function computing a
        strategy given an environment), then [step f] is the strategy which first apply [f] to 
        its input environment, and then apply the resulting strategy.
      *)

      val (<&>) : 'a t -> 'a t -> 'a t

      val (<?>) : string -> 'a t -> 'a t
    end
