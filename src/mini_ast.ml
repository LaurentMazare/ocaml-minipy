open! Base

type boolop =
  | And
  | Or
[@@deriving sexp]

type operator =
  | Add
  | Sub
  | Mult
  | MatMult
  | Div
  | Mod
  | Pow
  | LShift
  | RShift
  | BitOr
  | BitXor
  | BitAnd
  | FloorDiv
[@@deriving sexp]

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
  | BoolOp of
      { op : boolop
      ; values : expr list
      }
  | BinOp of
      { left : expr
      ; op : operator
      ; right : expr
      }
[@@deriving sexp]

type t = stmt list [@@deriving sexp]
