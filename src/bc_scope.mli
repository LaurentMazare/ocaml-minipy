open Base

type t

val create : unit -> t
val find : t -> string -> Bc_value.t option
val mem : t -> string -> bool
val set : t -> string -> Bc_value.t -> unit
val remove : t -> string -> unit
val of_alist_exn : (string * Bc_value.t) list -> t
