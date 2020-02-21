module Code : sig
  type t =
    { opcodes : (Bc_opcode.t * int) array
    ; consts : Bc_value.t array
    ; varnames : string array
    ; names : string array
    }
end

type t

type action =
  | No_action
  | End_of_code

val create : code:Code.t -> global_scope:Bc_scope.t -> builtins:Bc_scope.t -> t
val eval_step : t -> action
