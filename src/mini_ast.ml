open! Base

type stmt =
  | If of
      { test : expr
      ; body : stmt list
      ; orelse : stmt list
      }
  | Expr of expr
  | Assign of
      { targets : expr list
      ; value : expr
      }

and expr =
  | Num of int
  | Float of float
  | Str of string
  | Name of string
[@@deriving sexp]

type t = stmt list [@@deriving sexp]
