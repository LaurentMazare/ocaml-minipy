(* In this example, the Python type is extracted using the Python
   interpreter with the ast module (see examples/test.py), this
   is converted to json and read from this file.
*)
open! Base

(* These types are derived from:
   https://github.com/python/cpython/blob/3.7/Parser/Python.asdl
   It would be nicer to derive these types automatically from
   the asdl file.
*)
module Python37 = struct
  type identifier = string [@@deriving yojson, sexp]
  type constant = string [@@deriving yojson, sexp]

  type singleton =
    | None_ [@key "None"]
    | True
    | False
  [@@deriving yojson, sexp]

  type function_def =
    { name : identifier
    ; args : arguments
    ; body : stmt list
    ; decorator_list : expr list
    ; returns : expr option
    }

  and class_def =
    { name : identifier
    ; bases : expr list
    ; keywords : keyword list
    ; body : stmt list
    ; decorator_list : expr list
    }

  and for_ =
    { target : expr
    ; iter : expr
    ; body : stmt list
    ; orelse : stmt list
    }

  and slice =
    | Slice of
        { lower : expr option
        ; upper : expr option
        ; step : expr option
        }
    | ExtSlice of { dims : slice list }
    | Index of { value : expr }

  and arguments =
    | Arguments of
        { args : arg list
        ; vararg : arg option
        ; kwonlyargs : arg list
        ; kw_defaults : expr list
        ; kwarg : arg option
        ; defaults : expr list
        } [@name "arguments"]

  and arg =
    | Arg of
        { arg : identifier
        ; annotation : expr option
        } [@name "arg"]

  and boolop =
    | And
    | Or

  and operator =
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

  and expr_context =
    | Load
    | Store
    | Del
    | AugLoad
    | AugStore
    | Param

  and cmpop =
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

  and expr =
    | BoolOp of
        { op : boolop
        ; values : expr list
        }
    | BinOp of
        { left : expr
        ; op : operator
        ; right : expr
        }
    | Name of
        { id : identifier
        ; ctx : expr_context
        }
    | Num of { n : int }
    | Bytes of { s : string }
    | Str of { s : string }
    | JoinedStr of { values : expr list }
    | NameConstant of { value : singleton }
    | Ellipsis
    | Constant of { value : constant }
    | Attribute of
        { value : expr
        ; attr : identifier
        ; ctx : expr_context
        }
    | Subscript of
        { value : expr
        ; slice : slice
        ; ctx : expr_context
        }
    | Starred of
        { value : expr
        ; ctx : expr_context
        }
    | List of
        { elts : expr list
        ; ctx : expr_context
        }
    | Tuple of
        { elts : expr list
        ; ctx : expr_context
        }
    | Lambda of
        { args : arguments
        ; body : expr
        }
    | IfExp of
        { test : expr
        ; body : expr
        ; orelse : expr
        }
    | Dict of
        { keys : expr list
        ; values : expr list
        }
    | Set of { elts : expr list }
    | ListComp of
        { elt : expr
        ; generators : comprehension list
        }
    | SetComp of
        { elt : expr
        ; generators : comprehension list
        }
    | DictComp of
        { key : expr
        ; value : expr
        ; generators : comprehension list
        }
    | GeneratorExp of
        { elt : expr
        ; generators : comprehension list
        }
    | Await of { value : expr }
    | Yield of { value : expr option }
    | YieldFrom of { value : expr }
    | Compare of
        { left : expr
        ; ops : cmpop list
        ; comparators : expr list
        }
    | Call of
        { func : expr
        ; args : expr list
        ; keywords : keyword list
        }

  and comprehension =
    | Comprehension of
        { target : expr
        ; iter : expr
        ; ifs : expr list
        ; is_async : int
        }

  and keyword =
    | Keyword of
        { arg : identifier option
        ; value : expr
        }

  and withitem =
    | WithItem of
        { context_expr : expr
        ; optional_vals : expr option
        }

  and alias =
    | Alias of
        { name : identifier
        ; asname : identifier option
        }

  and excepthandler =
    | ExceptHandler of
        { type_ : expr option [@key "type"]
        ; name : identifier option
        ; body : stmt list
        }

  and stmt =
    | FunctionDef of function_def
    | AsyncFunctionDef of function_def
    | ClassDef of class_def
    | Return of { value : expr option }
    | Delete of { targets : expr list }
    | Assign of
        { targets : expr list
        ; value : expr
        }
    | AugAssign of
        { target : expr
        ; op : operator
        ; value : expr
        }
    | AnnAssign of
        { target : expr
        ; annotation : expr
        ; value : expr option
        ; simple : int
        }
    | For of for_
    | AsyncFor of for_
    | While of
        { test : expr
        ; body : stmt list
        ; orelse : stmt list
        }
    | If of
        { test : expr
        ; body : stmt list
        ; orelse : stmt
        }
    | With of
        { items : withitem list
        ; body : stmt list
        }
    | AsyncWith of
        { items : withitem list
        ; body : stmt list
        }
    | Raise of
        { exc : expr option
        ; cause : expr option
        }
    | Try of
        { body : stmt list
        ; handlers : excepthandler list
        ; orelse : stmt list
        ; finalbody : stmt list
        }
    | Assert of
        { test : expr
        ; msg : expr option
        }
    | Import of { names : alias list }
    | ImportFrom of
        { module_ : identifier option [@key "module"]
        ; names : alias list
        ; level : int option
        }
    | Global of { names : identifier list }
    | Nonlocal of { names : identifier list }
    | Expr of { value : expr }
    | Pass
    | Break
    | Continue

  and mod_ =
    | Module of { body : stmt list }
    | Expression of { body : expr }
  [@@deriving yojson, sexp]
end
[@warning "-30"]

let of_file filename =
  try
    Yojson.Safe.from_file filename
    |> Python37.mod__of_yojson
    |> Python37.sexp_of_mod_
    |> Sexp.to_string
    |> Stdio.printf "%s\n%!"
  with
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, yojson) ->
    Stdio.eprintf
      "Exn: %s\nJson: %s\n%!"
      (Exn.to_string exn)
      (Yojson.Safe.to_string yojson)

let () = of_file "/tmp/test.json"
