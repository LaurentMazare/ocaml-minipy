open! Base
open! Import

exception
  SyntaxError of
    { lineno : int
    ; error : string
    }

let errorf ~lineno fmt =
  Printf.ksprintf (fun error -> raise (SyntaxError { lineno; error })) fmt

module O : sig
  type label
  type t

  val op : ?arg:int -> lineno:int -> Bc_code.Opcode.t -> t
  val jump : lineno:int -> Bc_code.Opcode.t -> label -> t
  val label : unit -> label * t
  val to_opcodes : t list -> Bc_code.opcode_with_arg array
end = struct
  module Label : sig
    type t

    include Hashtbl.Key.S with type t := t

    val create : unit -> t
  end = struct
    include Int

    let create =
      let counter = ref 0 in
      fun () ->
        Int.incr counter;
        !counter
  end

  type label = Label.t

  type t =
    | Op of
        { opcode : Bc_code.Opcode.t
        ; arg : int
        ; jump_to : label option
        ; lineno : int
        }
    | Label of label

  let op ?(arg = 0) ~lineno opcode = Op { opcode; arg; jump_to = None; lineno }
  let jump ~lineno opcode jump_to = Op { opcode; arg = 0; jump_to = Some jump_to; lineno }

  let label () =
    let l = Label.create () in
    l, Label l

  let to_opcodes ts =
    let labels = Hashtbl.create (module Label) in
    let (_cnt : int) =
      List.fold ts ~init:0 ~f:(fun acc t ->
          match t with
          | Label l ->
            Hashtbl.add_exn labels ~key:l ~data:acc;
            acc
          | Op _ -> acc + 1)
    in
    List.filter_map ts ~f:(function
        | Op { opcode; arg; jump_to; lineno } ->
          let arg = Option.value_map jump_to ~f:(Hashtbl.find_exn labels) ~default:arg in
          Some { Bc_code.opcode; arg; lineno }
        | Label _ -> None)
    |> Array.of_list
end

module Id_set : sig
  type 'a t

  val create : unit -> 'a t
  val find_or_add : 'a t -> 'a -> int
  val to_array : 'a t -> 'a array
end = struct
  type 'a t = ('a, int) Hashtbl.Poly.t

  let create () = Hashtbl.Poly.create ()
  let find_or_add t v = Hashtbl.find_or_add t v ~default:(fun () -> Hashtbl.length t)

  let to_array t =
    Hashtbl.to_alist t
    |> List.sort ~compare:(fun (_k1, id1) (_k2, id2) -> Int.compare id1 id2)
    |> List.map ~f:fst
    |> Array.of_list
end

module Env = struct
  type loop_labels =
    { break : O.label
    ; continue : O.label
    }

  type t =
    { consts : Bc_value.t Id_set.t
    ; names : string Id_set.t
    ; varnames : string Id_set.t
    ; loop_labels : loop_labels option
    }

  let create () =
    { consts = Id_set.create ()
    ; names = Id_set.create ()
    ; varnames = Id_set.create ()
    ; loop_labels = None
    }

  let loop_env t ~break ~continue = { t with loop_labels = Some { continue; break } }
  let consts t = Id_set.to_array t.consts
  let names t = Id_set.to_array t.names
  let varnames t = Id_set.to_array t.varnames

  let load_const t const ~lineno =
    let id = Id_set.find_or_add t.consts const in
    O.op LOAD_CONST ~arg:id ~lineno

  let load_name t name ~lineno =
    (* TODO: use varnames for local variables *)
    let id = Id_set.find_or_add t.names name in
    O.op LOAD_NAME ~arg:id ~lineno

  let load_attr t name ~lineno =
    let id = Id_set.find_or_add t.names name in
    O.op LOAD_ATTR ~arg:id ~lineno

  let store_attr t name ~lineno =
    let id = Id_set.find_or_add t.names name in
    O.op STORE_ATTR ~arg:id ~lineno

  let delete_attr t name ~lineno =
    let id = Id_set.find_or_add t.names name in
    O.op DELETE_ATTR ~arg:id ~lineno

  let store_name t name ~lineno =
    (* TODO: use varnames for local variables *)
    let id = Id_set.find_or_add t.names name in
    O.op STORE_NAME ~arg:id ~lineno

  let delete_name t name ~lineno =
    let id = Id_set.find_or_add t.names name in
    O.op DELETE_NAME ~arg:id ~lineno
end

let binop_opcode : Ast.operator -> Bc_code.Opcode.t = function
  | Add -> BINARY_ADD
  | Sub -> BINARY_SUBTRACT
  | Mult -> BINARY_MULTIPLY
  | MatMult -> BINARY_MATRIX_MULTIPLY
  | Div -> BINARY_TRUE_DIVIDE
  | FloorDiv -> BINARY_FLOOR_DIVIDE
  | Mod -> BINARY_MODULO
  | Pow -> BINARY_POWER
  | LShift -> BINARY_LSHIFT
  | RShift -> BINARY_RSHIFT
  | BitOr -> BINARY_OR
  | BitXor -> BINARY_XOR
  | BitAnd -> BINARY_AND

let inplace_opcode : Ast.operator -> Bc_code.Opcode.t = function
  | Add -> INPLACE_ADD
  | Sub -> INPLACE_SUBTRACT
  | Mult -> INPLACE_MULTIPLY
  | MatMult -> INPLACE_MATRIX_MULTIPLY
  | Div -> INPLACE_TRUE_DIVIDE
  | FloorDiv -> INPLACE_FLOOR_DIVIDE
  | Mod -> INPLACE_MODULO
  | Pow -> INPLACE_POWER
  | LShift -> INPLACE_LSHIFT
  | RShift -> INPLACE_RSHIFT
  | BitOr -> INPLACE_OR
  | BitXor -> INPLACE_XOR
  | BitAnd -> INPLACE_AND

let unaryop_opcode : Ast.unaryop -> Bc_code.Opcode.t = function
  | UAdd -> UNARY_POSITIVE
  | USub -> UNARY_NEGATIVE
  | Not -> UNARY_NOT
  | Invert -> UNARY_INVERT

let rec compile_stmt env stmt =
  let lineno = (fst stmt.Ast.loc).pos_lnum in
  let op = O.op ~lineno in
  let jump = O.jump ~lineno in
  match (stmt.Ast.value : Ast.stmt) with
  | FunctionDef { name; args; body } ->
    let local_variables = Ast_utils.local_variables body in
    let body = compile body in
    let to_capture =
      (* This could be optimized by only capturing what is necessary. *)
      List.filter
        ((Env.names env |> Array.to_list) @ (Env.varnames env |> Array.to_list))
        ~f:(fun name -> not (Hash_set.mem local_variables name))
      |> List.dedup_and_sort ~compare:String.compare
    in
    List.concat_map args.Ast.kwonlyargs ~f:(fun (_, expr) -> compile_expr env expr)
    @ [ Env.load_const env (Bc_value.code body ~args ~to_capture) ~lineno
      ; Env.load_const env (Bc_value.str name) ~lineno
      ; op MAKE_FUNCTION
      ; Env.store_name env name ~lineno
      ]
  | ClassDef _ -> errorf ~lineno "Unsupported: ClassDef"
  | If { test; body; orelse } ->
    let test = compile_expr env test in
    let body = List.concat_map body ~f:(compile_stmt env) in
    if List.is_empty orelse
    then (
      let jump_to, label = O.label () in
      List.concat [ test; [ jump POP_JUMP_IF_FALSE jump_to ]; body; [ label ] ])
    else (
      let orelse = List.concat_map orelse ~f:(compile_stmt env) in
      let jump_to1, label1 = O.label () in
      let jump_to2, label2 = O.label () in
      List.concat
        [ test
        ; [ jump POP_JUMP_IF_FALSE jump_to1 ]
        ; body
        ; [ jump JUMP_ABSOLUTE jump_to2; label1 ]
        ; orelse
        ; [ label2 ]
        ])
  | For { target; iter; body; orelse } ->
    let jump_to1, label1 = O.label () in
    let jump_to2, label2 = O.label () in
    let jump_to3, label3 = O.label () in
    let iter = compile_expr env iter in
    let assign = assign env ~target in
    let body =
      let loop_env = Env.loop_env env ~break:jump_to3 ~continue:jump_to2 in
      List.concat_map body ~f:(compile_stmt loop_env)
    in
    let orelse = List.concat_map orelse ~f:(compile_stmt env) in
    List.concat
      [ iter
      ; [ op GET_ITER; label2; jump FOR_ITER jump_to1 ]
      ; assign
      ; body
      ; [ jump JUMP_ABSOLUTE jump_to2; label1 ]
      ; orelse
      ; [ label3 ]
      ]
  | While { test; body; orelse } ->
    let test = compile_expr env test in
    let jump_to1, label1 = O.label () in
    let jump_to2, label2 = O.label () in
    let jump_to3, label3 = O.label () in
    let loop_env = Env.loop_env env ~break:jump_to3 ~continue:jump_to2 in
    let body = List.concat_map body ~f:(compile_stmt loop_env) in
    let orelse = List.concat_map orelse ~f:(compile_stmt env) in
    List.concat
      [ [ label2 ]
      ; test
      ; [ jump POP_JUMP_IF_FALSE jump_to1 ]
      ; body
      ; [ jump JUMP_ABSOLUTE jump_to2; label1 ]
      ; orelse
      ; [ label3 ]
      ]
  | Raise _ -> errorf ~lineno "Unsupported: Raise"
  | Try _ -> errorf ~lineno "Unsupported: Try"
  | With _ -> errorf ~lineno "Unsupported: With"
  | Assert _ -> errorf ~lineno "Unsupported: Assert"
  | Import _ -> errorf ~lineno "Unsupported: Import"
  | ImportFrom _ -> errorf ~lineno "Unsupported: ImportFrom"
  | Expr { value } -> compile_expr env value @ [ op POP_TOP ]
  | Assign { targets; value } -> assign_targets env ~targets ~value
  | AugAssign { target; op = op_; value } -> aug_assign env ~target ~op_ ~value
  | Return { value } ->
    let load_value =
      match value with
      | None -> [ Env.load_const env Bc_value.none ~lineno ]
      | Some expr -> compile_expr env expr
    in
    load_value @ [ op RETURN_VALUE ]
  | Delete { targets } -> delete env targets
  | Pass -> [ op NOP ]
  | Break ->
    (match env.loop_labels with
    | None -> errorf ~lineno "break not in a loop"
    | Some { break; continue = _ } -> [ jump JUMP_ABSOLUTE break ])
  | Continue ->
    (match env.loop_labels with
    | None -> errorf ~lineno "continue not in a loop"
    | Some { break = _; continue } -> [ jump JUMP_ABSOLUTE continue ])

and compile_expr env expr =
  let lineno = (fst expr.Ast.loc).pos_lnum in
  let op = O.op ~lineno in
  let jump = O.jump ~lineno in
  match (expr.Ast.value : Ast.expr) with
  | None_ -> [ Env.load_const env Bc_value.none ~lineno ]
  | Bool b -> [ Env.load_const env (Bc_value.bool b) ~lineno ]
  | Num n -> [ Env.load_const env (Bc_value.int n) ~lineno ]
  | Float f -> [ Env.load_const env (Bc_value.float f) ~lineno ]
  | Str s -> [ Env.load_const env (Bc_value.str s) ~lineno ]
  | Name name -> [ Env.load_name env name ~lineno ]
  | List exprs ->
    let exprs = Array.to_list exprs in
    List.concat_map exprs ~f:(compile_expr env)
    @ [ op BUILD_LIST ~arg:(List.length exprs) ]
  | Dict _ -> errorf ~lineno "Unsupported: Dict"
  | ListComp { elt; generators } ->
    let depth = List.length generators in
    let generators =
      List.rev generators
      |> List.fold
           ~init:(compile_expr env elt @ [ op LIST_APPEND ~arg:(1 + depth) ])
           ~f:(fun body { Ast.target; iter; ifs } ->
             let jump_to1, label1 = O.label () in
             let jump_to2, label2 = O.label () in
             let iter = compile_expr env iter in
             let assign = assign env ~target in
             List.concat
               [ iter
               ; [ op GET_ITER; label2; jump FOR_ITER jump_to1 ]
               ; assign
               ; List.concat_map ifs ~f:(fun expr ->
                     compile_expr env expr @ [ jump POP_JUMP_IF_FALSE jump_to2 ])
               ; body
               ; [ jump JUMP_ABSOLUTE jump_to2; label1 ]
               ])
    in
    op BUILD_LIST ~arg:0 :: generators
  | Tuple exprs ->
    let exprs = Array.to_list exprs in
    List.concat_map exprs ~f:(compile_expr env)
    @ [ op BUILD_TUPLE ~arg:(List.length exprs) ]
  | Lambda _ -> errorf ~lineno "Unsupported: Lambda"
  | BoolOp { op = And; values } ->
    let values = List.map values ~f:(compile_expr env) in
    let jump_to, label = O.label () in
    let code =
      List.intersperse values ~sep:[ jump JUMP_IF_FALSE_OR_POP jump_to ] |> List.concat
    in
    code @ [ label ]
  | BoolOp { op = Or; values } ->
    let values = List.map values ~f:(compile_expr env) in
    let jump_to, label = O.label () in
    let code =
      List.intersperse values ~sep:[ jump JUMP_IF_TRUE_OR_POP jump_to ] |> List.concat
    in
    code @ [ label ]
  | BinOp { left; op = op_; right } ->
    List.concat
      [ compile_expr env left; compile_expr env right; [ op (binop_opcode op_) ] ]
  | UnaryOp { op = op_; operand } ->
    compile_expr env operand @ [ op (unaryop_opcode op_) ]
  | IfExp { test; body; orelse } ->
    let test = compile_expr env test in
    let body = compile_expr env body in
    let orelse = compile_expr env orelse in
    let jump_to1, label1 = O.label () in
    let jump_to2, label2 = O.label () in
    List.concat
      [ test
      ; [ jump POP_JUMP_IF_FALSE jump_to1 ]
      ; body
      ; [ jump JUMP_ABSOLUTE jump_to2; label1 ]
      ; orelse
      ; [ label2 ]
      ]
  | Compare { left; ops_and_exprs = [ (cmpop, expr) ] } ->
    let left = compile_expr env left in
    let expr = compile_expr env expr in
    let arg = Bc_code.int_of_cmpop cmpop in
    left @ expr @ [ op COMPARE_OP ~arg ]
  | Compare { left; ops_and_exprs } ->
    let left = compile_expr env left in
    let jump_to, label = O.label () in
    let nops_and_exprs = List.length ops_and_exprs in
    left
    @ List.concat_mapi ops_and_exprs ~f:(fun index (cmpop, expr) ->
          let arg = Bc_code.int_of_cmpop cmpop in
          let expr =
            compile_expr env expr @ [ op DUP_TOP; op ROT_THREE; op COMPARE_OP ~arg ]
          in
          let tail =
            if index = nops_and_exprs - 1
            then [ label ]
            else [ jump JUMP_IF_FALSE_OR_POP jump_to ]
          in
          expr @ tail)
    @ [ op ROT_TWO; op POP_TOP ]
  | Call { func; args; keywords = [] } ->
    let func = compile_expr env func in
    let args = List.map args ~f:(compile_expr env) in
    List.concat (func :: args) @ [ op CALL_FUNCTION ~arg:(List.length args) ]
  | Call { func; args; keywords } ->
    let func = compile_expr env func in
    let args = List.map args ~f:(compile_expr env) in
    let kwarg_names =
      List.map keywords ~f:(fun (name, _) -> Bc_value.str name)
      |> Array.of_list
      |> Bc_value.tuple
    in
    let kwargs = List.map keywords ~f:(fun (_, expr) -> compile_expr env expr) in
    List.concat ((func :: args) @ kwargs)
    @ [ Env.load_const env kwarg_names ~lineno
      ; op CALL_FUNCTION_KW ~arg:(List.length args + List.length kwargs)
      ]
  | Attribute { value; attr } ->
    let value = compile_expr env value in
    value @ [ Env.load_attr env attr ~lineno ]
  | Subscript { value; slice } ->
    let value = compile_expr env value in
    let slice = compile_expr env slice in
    value @ slice @ [ op BINARY_SUBSCR ]

and delete env targets =
  List.concat_map targets ~f:(fun target ->
      let lineno = (fst target.Ast.loc).pos_lnum in
      let op = O.op ~lineno in
      match target.value with
      | None_ | Bool _ | Num _ | Float _ | Str _ -> errorf ~lineno "can't delete constant"
      | Dict _ -> errorf ~lineno "can't delete dict"
      | BoolOp _ | BinOp _ | UnaryOp _ | IfExp _ | Compare _ ->
        errorf ~lineno "can't delete operator"
      | Call _ -> errorf ~lineno "can't delete function call"
      | ListComp _ -> errorf ~lineno "can't delete comprehension"
      | Lambda _ -> errorf ~lineno "can't delete lambda"
      | Name name -> [ Env.delete_name env name ~lineno ]
      | Tuple _exprs -> errorf ~lineno "can't delete tuple"
      | List _exprs -> errorf ~lineno "can't delete list"
      | Attribute { value = value_attr; attr } ->
        let value_attr = compile_expr env value_attr in
        List.concat [ value_attr; [ Env.delete_attr env attr ~lineno ] ]
      | Subscript { value = value_attr; slice } ->
        let value_attr = compile_expr env value_attr in
        let slice = compile_expr env slice in
        List.concat [ value_attr; slice; [ op DELETE_SUBSCR ] ])

and aug_assign env ~target ~op_ ~value =
  let value = compile_expr env value in
  let lineno = (fst target.Ast.loc).pos_lnum in
  let op = O.op ~lineno in
  match target.value with
  | None_ | Bool _ | Num _ | Float _ | Str _ ->
    errorf ~lineno "can't augmented assign to constant"
  | Dict _ -> errorf ~lineno "can't augmented assign to dict"
  | BoolOp _ | BinOp _ | UnaryOp _ | IfExp _ | Compare _ ->
    errorf ~lineno "can't augmented assign to operator"
  | Call _ -> errorf ~lineno "can't augmented assign to function call"
  | ListComp _ -> errorf ~lineno "can't augmented assign to comprehension"
  | Lambda _ -> errorf ~lineno "can't augmented assign to lambda"
  | Name name ->
    List.concat
      [ [ Env.load_name env name ~lineno ]
      ; value
      ; [ op (inplace_opcode op_); Env.store_name env name ~lineno ]
      ]
  | Tuple _exprs -> errorf ~lineno "can't augmented assign to tuple"
  | List _exprs -> errorf ~lineno "can't augmented assign to list"
  | Attribute { value = value_attr; attr } ->
    let value_attr = compile_expr env value_attr in
    List.concat
      [ value_attr
      ; [ op DUP_TOP; Env.load_attr env attr ~lineno ]
      ; value
      ; [ op (inplace_opcode op_); op ROT_TWO; Env.store_attr env attr ~lineno ]
      ]
  | Subscript { value = value_attr; slice } ->
    let value_attr = compile_expr env value_attr in
    let slice = compile_expr env slice in
    List.concat
      [ value_attr
      ; slice
      ; [ op DUP_TOP_TWO; op BINARY_SUBSCR ]
      ; value
      ; [ op (inplace_opcode op_); op ROT_THREE; op STORE_SUBSCR ]
      ]

and assign env ~target =
  let rec loop expr =
    let lineno = (fst expr.Ast.loc).pos_lnum in
    let op = O.op ~lineno in
    match expr.Ast.value with
    | Ast.None_ | Bool _ | Num _ | Float _ | Str _ ->
      errorf ~lineno "can't assign to constant"
    | Dict _ -> errorf ~lineno "can't assign to dict"
    | BoolOp _ | BinOp _ | UnaryOp _ | IfExp _ | Compare _ ->
      errorf ~lineno "can't assign to operator"
    | Call _ -> errorf ~lineno "can't assign to function call"
    | ListComp _ -> errorf ~lineno "can't assign to comprehension"
    | Lambda _ -> errorf ~lineno "can't assign to lambda"
    | Name name -> [ Env.store_name env name ~lineno ]
    | Tuple exprs | List exprs ->
      let nexprs = Array.length exprs in
      let exprs = Array.to_list exprs in
      op UNPACK_SEQUENCE ~arg:nexprs :: List.concat_map exprs ~f:loop
    | Attribute { value; attr } ->
      let value = compile_expr env value in
      value @ [ Env.store_attr env attr ~lineno ]
    | Subscript { value; slice } ->
      let value = compile_expr env value in
      let slice = compile_expr env slice in
      value @ slice @ [ op STORE_SUBSCR ]
  in
  loop target

and assign_targets env ~targets ~value =
  let lineno = (fst value.Ast.loc).pos_lnum in
  let op = O.op ~lineno in
  let value = compile_expr env value in
  let dups = List.init (List.length targets - 1) ~f:(fun _ -> op DUP_TOP) in
  let targets = List.concat_map targets ~f:(fun target -> assign env ~target) in
  value @ dups @ targets

and compile (ast : Ast.t) =
  let env = Env.create () in
  let opcodes = List.concat_map ast ~f:(compile_stmt env) |> O.to_opcodes in
  let filename =
    match ast with
    | [] -> "unknown"
    | stmt :: _ -> (fst stmt.loc).pos_fname
  in
  { Bc_code.opcodes
  ; consts = Env.consts env
  ; varnames = Env.varnames env
  ; names = Env.names env
  ; filename
  }
