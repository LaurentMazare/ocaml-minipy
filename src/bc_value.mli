open Base

type t =
  | None
  | Bool of bool
  | Int of Z.t
  | Float of float
  | Tuple of t array
  | List of t array
  | Dict of (t, t) Hashtbl.Poly.t
  | Str of string

val to_string : ?escape_special_chars:bool -> t -> string
