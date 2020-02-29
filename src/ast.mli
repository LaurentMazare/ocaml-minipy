open! Base

type boolop =
  | And
  | Or
[@@deriving sexp]

type unaryop =
  | Invert
  | Not
  | UAdd
  | USub
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

type cmpop =
  | Eq
  | NotEq
  | Lt
  | LtE
  | Gt
  | GtE
  | Is
  | IsNot
  | In
  | NotIn
[@@deriving sexp]

type 'a with_loc =
  { loc : Lexing.position * Lexing.position
  ; value : 'a
  }
[@@deriving sexp]

type stmt =
  | FunctionDef of
      { name : string
      ; args : arguments
      ; body : stmt with_loc list
      }
  | ClassDef of
      { name : string
      ; args : arguments
      ; body : stmt with_loc list
      }
  | If of
      { test : expr with_loc
      ; body : stmt with_loc list
      ; orelse : stmt with_loc list
      }
  | For of
      { target : expr with_loc
      ; iter : expr with_loc
      ; body : stmt with_loc list
      ; orelse : stmt with_loc list
      }
  | While of
      { test : expr with_loc
      ; body : stmt with_loc list
      ; orelse : stmt with_loc list
      }
  | Raise of
      { exc : expr with_loc option
      ; cause : expr with_loc option
      }
  | Try of
      { body : stmt with_loc list
      ; handlers : excepthandler list
      ; orelse : stmt with_loc list
      ; finalbody : stmt with_loc list
      }
  | With of
      { body : stmt with_loc list
      ; context : expr with_loc
      ; vars : expr with_loc option
      }
  | Assert of
      { test : expr with_loc
      ; msg : expr with_loc option
      }
  | Import of importname list
  | ImportFrom of string * importname list
  | Expr of { value : expr with_loc }
  | Assign of
      { targets : expr with_loc list
      ; value : expr with_loc
      }
  | AugAssign of
      { target : expr with_loc
      ; op : operator
      ; value : expr with_loc
      }
  | Return of { value : expr with_loc option }
  | Delete of { targets : expr with_loc list }
  | Pass
  | Break
  | Continue

and expr =
  | None_
  | Bool of bool
  | Num of Z.t
  | Float of float
  | Str of string
  | Name of string
  | List of expr with_loc array
  | Dict of { key_values : (expr with_loc * expr with_loc) list }
  | ListComp of
      { elt : expr with_loc
      ; generators : comprehension list
      }
  | Tuple of expr with_loc array
  | Lambda of
      { args : arguments
      ; body : expr with_loc
      }
  | BoolOp of
      { op : boolop
      ; values : expr with_loc list
      }
  | BinOp of
      { left : expr with_loc
      ; op : operator
      ; right : expr with_loc
      }
  | UnaryOp of
      { op : unaryop
      ; operand : expr with_loc
      }
  | IfExp of
      { test : expr with_loc
      ; body : expr with_loc
      ; orelse : expr with_loc
      }
  | Compare of
      { left : expr with_loc
      ; ops_and_exprs : (cmpop * expr with_loc) list
      }
  | Call of
      { func : expr with_loc
      ; args : expr with_loc list
      ; keywords : (string * expr with_loc) list
      }
  | Attribute of
      { value : expr with_loc
      ; attr : string
      }
  | Subscript of
      { value : expr with_loc
      ; slice : expr with_loc (* TODO: proper slices *)
      }

and comprehension =
  { target : expr with_loc
  ; iter : expr with_loc
  ; ifs : expr with_loc list
  }

and arguments =
  { args : string list
  ; vararg : string option
  ; kwonlyargs : (string * expr with_loc) list
  ; kwarg : string option
  }

and excepthandler =
  { type_ : expr with_loc option
  ; name : string option
  ; body : stmt with_loc list
  }

and importname =
  { import_name : string
  ; as_name : string option
  }
[@@deriving sexp]

type t = stmt with_loc list [@@deriving sexp]
