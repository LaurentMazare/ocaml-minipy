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
  | FunctionDef of
      { name : string
      ; args : string list (* TODO: other args *)
      ; body : stmt list
      }
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
  | Return of { value : expr option }
  | Delete of { targets : expr list }

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
  | IfExp of
      { test : expr
      ; body : expr
      ; orelse : expr
      }
  | Call of
      { func : expr
      ; args : expr list (* TODO; keywords : keyword list *)
      }
[@@deriving sexp]

type t = stmt list [@@deriving sexp]
