open! Base

type builtins = (string, Value.builtin_fn, String.comparator_witness) Map.t

val simple_eval : ?builtins:builtins -> Ast.t -> unit
val simple_eval_expr : ?builtins:builtins -> Ast.expr Ast.with_loc -> Value.t

module Env : sig
  type t = Value.env

  val empty : ?builtins:builtins -> unit -> t
end

val eval_stmts : Env.t -> Ast.t -> unit
val eval_expr : Env.t -> Ast.expr Ast.with_loc -> Value.t
