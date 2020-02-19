type t

val create : unit -> t
val eval : t -> Bc_frame.Code.t -> unit
