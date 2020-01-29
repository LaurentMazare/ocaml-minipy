open! Base

(* These types are derived from:
   https://github.com/python/cpython/blob/3.7/Parser/Python.asdl
*)
module Python37 = struct
  type identifier = string [@@deriving yojson, sexp]

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

  and keyword =
    { arg : identifier option
    ; value : expr
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
    |> Stdio.printf ">%s\n%!"
  with
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, yojson) ->
    Stdio.eprintf
      "Exn: %s\nJson: %s\n%!"
      (Exn.to_string exn)
      (Yojson.Safe.to_string yojson)
