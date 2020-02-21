type t

type action =
  | No_action
  | End_of_code

val create : code:Bc_value.code -> global_scope:Bc_scope.t -> builtins:Bc_scope.t -> t
val eval_step : t -> action
