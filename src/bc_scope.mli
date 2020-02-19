open Base

type t

val create : unit -> t
val find : t -> string -> Bc_value.t option
val set : t -> string -> Bc_value.t -> unit
val remove : t -> string -> unit
