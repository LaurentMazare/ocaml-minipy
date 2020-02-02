open! Base

module Type_ : sig
  type t =
    | None_t
    | Bool
    | Int
    | Float
    | Tuple
    | Dict
    | List
    | Str
    | Builtin_fn
    | Function
  [@@deriving sexp]
end

type value =
  | Val_none
  | Val_bool of bool
  | Val_int of int
  | Val_float of float
  | Val_tuple of value array
  | Val_list of value array
  | Val_dict of (value, value) Hashtbl.Poly.t
  | Val_str of string
  | Val_builtin_fn of (value list -> value)
  | Val_function of
      { args : string list
      ; body : Ast.stmt list
      }
[@@deriving sexp]

val type_of : value -> Type_.t

type builtins = (string, value list -> value, String.comparator_witness) Map.t

val default_builtins : builtins
val simple_eval : ?builtins:builtins -> Ast.t -> unit
val simple_eval_expr : ?builtins:builtins -> Ast.expr -> value
