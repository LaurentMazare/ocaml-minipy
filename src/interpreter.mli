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

module Value : sig
  type fn =
    { args : Ast.arguments
    ; body : Ast.stmt list
    }
  [@@deriving sexp]

  type t =
    | Val_none
    | Val_bool of bool
    | Val_int of int
    | Val_float of float
    | Val_tuple of t array
    | Val_list of t array
    | Val_dict of (t, t) Hashtbl.Poly.t
    | Val_str of string
    | Val_builtin_fn of builtin_fn
    | Val_function of fn

  and builtin_fn = t list -> (string, t) Hashtbl.t -> t [@@deriving sexp]

  val type_ : t -> Type_.t
end

type builtins = (string, Value.builtin_fn, String.comparator_witness) Map.t

val default_builtins : builtins
val simple_eval : ?builtins:builtins -> Ast.t -> unit
val simple_eval_expr : ?builtins:builtins -> Ast.expr -> Value.t
