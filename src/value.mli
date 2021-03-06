open Base
open Ast

module Class_id : sig
  type t

  val create : unit -> t
  val equal : t -> t -> bool
end

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

type 'a dict = ('a, 'a) Hashtbl.Poly.t

type t =
  | Val_none
  | Val_bool of bool
  | Val_int of Z.t
  | Val_float of float
  | Val_tuple of t array
  | Val_list of t Queue.t
  | Val_dict of t dict
  | Val_str of string
  | Val_class of cls
  | Val_object of
      { cls : cls
      ; attrs : (string, t) Hashtbl.t
      }
  | Val_builtin_fn of builtin_fn
  | Val_function of fn

and builtin_fn = interp -> t list -> (string, t) Hashtbl.t -> t

and builtins = (string, builtin_fn, String.comparator_witness) Map.t

and env =
  { scope : (string, t) Hashtbl.t
  ; prev_env : env option
  ; local_variables : string Hash_set.t
  ; builtins : builtins
  }

and fn =
  { args : Ast.arguments
  ; env : env
  ; body : Ast.t
  ; method_self : t option
  }

and cls =
  { name : string
  ; attrs : (string, t) Hashtbl.t
  ; parent_class : cls option
  ; id : Class_id.t
  }

and interp =
  { call_method : t -> string -> t list -> t
  ; has_method : t -> string -> bool
  }

module Exception : sig
  val base_exception_cls : cls
  val exception_cls : cls
  val index_error_cls : cls
  val value_error_cls : cls
  val not_implemented_cls : cls
  val assertion_error_cls : cls
  val exceptions : (string * t) list
end

exception
  Raise of
    { exc : t option
    ; cause : t option
    }

val raise_ : exc:t option -> cause:t option -> 'a
val raise_cls : cls -> args:t list -> 'a
val type_ : t -> Type_.t
val to_string : ?escape_special_chars:bool -> t -> string
val to_int : t -> Z.t
val to_bool : t -> bool
val to_iterable : t -> t array
val apply_subscript : value:t -> index:t -> t
val apply_subscript_assign : lvalue:t -> slice:t -> rvalue:t -> unit
val apply_unary_op : unaryop -> t -> t
val apply_op : operator -> t -> t -> t
val apply_comp : cmpop -> t -> t -> bool
val none : t
val str : string -> t
val list : t Queue.t -> t
val bool : bool -> t
val int : Z.t -> t
val float : float -> t
val tuple : t array -> t
val dict : t dict -> t
val fn : fn -> t
val cannot_be_interpreted_as : t -> string -> 'a
val type_as_string : t -> string
val is_subclass : cls -> target_class:cls -> bool
val is_instance : t -> target_class:cls -> bool
val is_instance_or_subclass : t -> target_class:cls -> bool
