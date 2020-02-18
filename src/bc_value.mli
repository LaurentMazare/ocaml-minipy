type t =
  | None
  | Bool of bool
  | Int of Z.t
  | Float of float
  | Tuple of t array
