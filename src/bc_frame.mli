open Base

type t

type action =
  | Continue (* Different from loop continue. *)
  | Call_fn of
      { code : Bc_value.code
      ; local_scope : Bc_scope.t
      }
  | Return of Bc_value.t

val top_frame : code:Bc_value.code -> global_scope:Bc_scope.t -> builtins:Bc_scope.t -> t
val call_frame : t -> code:Bc_value.code -> local_scope:Bc_scope.t -> t
val eval_step : t -> action
val function_call_returned : t -> Bc_value.t -> unit
val stack : t -> Bc_value.t Stack.t
