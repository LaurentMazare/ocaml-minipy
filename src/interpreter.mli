open! Base

type value =
  | Val_none
  | Val_bool of bool
  | Val_int of int
  | Val_float of float
  | Val_tuple of value array
  | Val_list of value array
  | Val_str of string
  | Val_function of
      { args : string list
      ; body : Ast.stmt list
      }
[@@deriving sexp]

val simple_eval : Ast.t -> unit
