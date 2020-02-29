open! Base
open! Import

exception
  SyntaxError of
    { lineno : int
    ; error : string
    }

val compile : Ast.t -> Bc_value.code
