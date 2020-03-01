open Base
open Import

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
    | Code
    | Iterator

  val to_string : t -> string
end

type t =
  | None
  | Bool of bool
  | Int of Z.t
  | Float of float
  | Tuple of t array
  | List of t Queue.t
  | Dict of (t, t) Hashtbl.Poly.t
  | Str of string
  | Builtin_fn of
      { name : string
      ; fn : t list -> t String_map.t -> t
      }
  | Function of fn
  | Code of
      { code : t Bc_code.t
      ; args : Ast.arguments
      ; to_capture : string list
      }
  | Iterator of { next : unit -> t option }
  | Class of cls
  | Object of
      { cls : cls
      ; attrs : (string, t) Hashtbl.t
      }

and cls =
  { cls_name : string
  ; attrs : (string, t) Hashtbl.t
  ; parent_class : cls option
  ; id : Class_id.t
  }

and fn =
  { name : string
  ; code : t Bc_code.t
  ; args : Ast.arguments
  ; defaults : (string * t) list
  ; captured : (string * t) list
  ; method_self : t option
  }
[@@deriving sexp_of]

type code = t Bc_code.t [@@deriving sexp_of]

val type_ : t -> Type_.t
val type_as_string : t -> string
val to_string : ?escape_special_chars:bool -> t -> string
val str_exn : t -> string
val code_exn : t -> code * Ast.arguments * string list
val none : t
val bool : bool -> t
val int : Z.t -> t
val float : float -> t
val str : string -> t
val tuple : t array -> t
val list : t Queue.t -> t
val code : code -> args:Ast.arguments -> to_capture:string list -> t
val iterator : next:(unit -> t option) -> t
val to_bool : t -> bool
val to_int : t -> Z.t
val to_float : t -> float
val to_iterable : t -> t
val cannot_be_interpreted_as : t -> string -> 'a
val is_subclass : cls -> target_class:cls -> bool
val is_instance : t -> target_class:cls -> bool
val is_instance_or_subclass : t -> target_class:cls -> bool
