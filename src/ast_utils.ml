open! Base
open! Import

(* Local variables are variables that are assigned in the body. *)
let local_variables body =
  let local_variables = Hash_set.create (module String) in
  let rec loop stmt =
    match stmt.Ast.value with
    | Ast.If { test = _; body; orelse } | While { test = _; body; orelse } ->
      List.iter body ~f:loop;
      List.iter orelse ~f:loop
    | For { target; iter = _; body; orelse } ->
      loop_expr target;
      List.iter body ~f:loop;
      List.iter orelse ~f:loop
    | Try { body; orelse; finalbody; handlers } ->
      List.iter body ~f:loop;
      List.iter orelse ~f:loop;
      List.iter finalbody ~f:loop;
      List.iter handlers ~f:(fun handler -> List.iter handler.body ~f:loop)
    | Assign { targets; value = _ } -> List.iter targets ~f:loop_expr
    | AugAssign { target = _; op = _; value = _ } -> ()
    (* Augmented assign does not create a new bindings but replaces an existing one. *)
    | ClassDef { name; _ } | FunctionDef { name; _ } -> Hash_set.add local_variables name
    | With { body; context = _; vars } ->
      List.iter body ~f:loop;
      Option.iter vars ~f:loop_expr
    | Import importnames | ImportFrom (_, importnames) ->
      List.iter importnames ~f:(fun { Ast.import_name; as_name } ->
          let as_name = Option.value as_name ~default:import_name in
          Hash_set.add local_variables as_name)
    | Assert _ | Return _ | Delete _ | Expr _ | Raise _ | Break | Continue | Pass -> ()
  and loop_expr expr =
    match expr.Ast.value with
    | Name name -> Hash_set.add local_variables name
    | List l | Tuple l -> Array.iter l ~f:loop_expr
    | None_
    | ListComp _
    | Dict _
    | Lambda _
    | BoolOp _
    | BinOp _
    | UnaryOp _
    | IfExp _
    | Compare _
    | Call _
    | Attribute _
    | Subscript _
    | Bool _
    | Num _
    | Float _
    | Str _ -> ()
  in
  List.iter body ~f:loop;
  local_variables
