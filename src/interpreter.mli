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
    | Object
    | Class
  [@@deriving sexp]
end

module Value : sig
  type 'a dict = ('a, 'a) Hashtbl.Poly.t

  type t =
    | Val_none
    | Val_bool of bool
    | Val_int of int
    | Val_float of float
    | Val_tuple of t array
    | Val_list of t array
    | Val_dict of t dict
    | Val_str of string
    | Val_class of cls
    | Val_object of
        { cls : cls
        ; attrs : ((string, t) Hashtbl.t[@sexp.opaque])
        }
    | Val_builtin_fn of builtin_fn
    | Val_function of fn

  and builtin_fn = t list -> (string, t) Hashtbl.t -> t

  and builtins = ((string, builtin_fn, String.comparator_witness) Map.t[@sexp.opaque])

  and env =
    { scope : ((string, t) Hashtbl.t[@sexp.opaque])
    ; prev_env : env option
    ; local_variables : (string Hash_set.t[@sexp.opaque])
    ; builtins : builtins
    }

  and fn =
    { args : Ast.arguments
    ; env : env
    ; body : Ast.stmt list
    ; method_self : t option
    }

  and cls =
    { name : string
    ; attrs : ((string, t) Hashtbl.t[@sexp.opaque])
    }
  [@@deriving sexp]

  val type_ : t -> Type_.t
  val to_string : ?escape_special_chars:bool -> t -> string
end

type builtins = (string, Value.builtin_fn, String.comparator_witness) Map.t

val default_builtins : builtins
val simple_eval : ?builtins:builtins -> Ast.t -> unit
val simple_eval_expr : ?builtins:builtins -> Ast.expr -> Value.t

module Env : sig
  type t = Value.env

  val empty : builtins:builtins -> t
end

val eval_stmts : Env.t -> Ast.t -> unit
val eval_expr : Env.t -> Ast.expr -> Value.t

exception RuntimeError of string
