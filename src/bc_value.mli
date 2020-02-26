open Base

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
  | List of t array
  | Dict of (t, t) Hashtbl.Poly.t
  | Str of string
  | Builtin_fn of
      { name : string
      ; fn : t list -> t
      }
  | Function of
      { name : string
      ; code : t Bc_code.t
      ; args : Ast.arguments
      ; defaults : t list
      }
  | Code of
      { code : t Bc_code.t
      ; args : Ast.arguments
      }
  | Iterator of { next : unit -> t option }
[@@deriving sexp_of]

type code = t Bc_code.t [@@deriving sexp_of]

val type_ : t -> Type_.t
val to_string : ?escape_special_chars:bool -> t -> string
val str_exn : t -> string
val code_exn : t -> code * Ast.arguments
val none : t
val bool : bool -> t
val int : Z.t -> t
val float : float -> t
val str : string -> t
val tuple : t array -> t
val list : t array -> t
val code : code -> args:Ast.arguments -> t
val iterator : next:(unit -> t option) -> t
val to_bool : t -> bool
val to_int : t -> Z.t
val to_float : t -> float
val to_iterable : t -> t
val cannot_be_interpreted_as : t -> string -> 'a
