module Code : sig
  type t =
    { opcodes : (Bc_opcode.t * int) array
    ; consts : Bc_value.t array
    ; varnames : string array
    ; names : string array
    }
end

type t

val create : code:Code.t -> global_scope:Bc_scope.t -> builtins:Bc_scope.t -> t
val eval : t -> Bc_value.t option
