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

val type_ : t -> Type_.t
val to_string : ?escape_special_chars:bool -> t -> string

type code = t Bc_code.t
