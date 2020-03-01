open Base

type filename_and_lineno =
  { filename : string
  ; lineno : int option
  }
[@@deriving sexp]

type backtrace = filename_and_lineno list [@@deriving sexp]

exception Exn_with_backtrace of Exn.t * backtrace

val eval : Bc_value.code -> unit
