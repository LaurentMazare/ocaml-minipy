open! Base
open! Import

module O : sig
  type label
  type t

  val op : ?arg:int -> Bc_code.Opcode.t -> t
  val jump : Bc_code.Opcode.t -> label -> t
  val label : unit -> label * t
  val to_opcodes : t list -> (Bc_code.Opcode.t * int) array
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
        }
    | Label of label

  let op ?(arg = 0) opcode = Op { opcode; arg; jump_to = None }
  let jump opcode jump_to = Op { opcode; arg = 0; jump_to = Some jump_to }

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
        | Op { opcode; arg; jump_to } ->
          let arg = Option.value_map jump_to ~f:(Hashtbl.find_exn labels) ~default:arg in
          Some (opcode, arg)
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
  type t =
    { consts : Bc_value.t Id_set.t
    ; names : string Id_set.t
    ; varnames : string Id_set.t
    }

  let create () =
    { consts = Id_set.create (); names = Id_set.create (); varnames = Id_set.create () }

  let consts t = Id_set.to_array t.consts
  let names t = Id_set.to_array t.names
  let varnames t = Id_set.to_array t.varnames

  let load_const t const =
    let id = Id_set.find_or_add t.consts const in
    O.op LOAD_CONST ~arg:id

  let load_name t name =
    (* TODO: use varnames for local variables *)
    let id = Id_set.find_or_add t.names name in
    O.op LOAD_NAME ~arg:id

  let load_attr t name =
    let id = Id_set.find_or_add t.names name in
    O.op LOAD_ATTR ~arg:id

  let store_attr t name =
    let id = Id_set.find_or_add t.names name in
    O.op STORE_ATTR ~arg:id

  let store_name t name =
    (* TODO: use varnames for local variables *)
    let id = Id_set.find_or_add t.names name in
    O.op STORE_NAME ~arg:id
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

let unaryop_opcode : Ast.unaryop -> Bc_code.Opcode.t = function
  | UAdd -> UNARY_POSITIVE
  | USub -> UNARY_NEGATIVE
  | Not -> UNARY_NOT
  | Invert -> UNARY_INVERT

let rec compile_stmt env stmt =
  match (stmt : Ast.stmt) with
  | FunctionDef { name; args; body } ->
    let body = compile body in
    [ Env.load_const env (Bc_value.code body ~args)
    ; Env.load_const env (Bc_value.str name)
    ; O.op MAKE_FUNCTION
    ; Env.store_name env name
    ]
  | ClassDef _ -> failwith "Unsupported: ClassDef"
  | If { test; body; orelse } ->
    let test = compile_expr env test in
    let body = List.concat_map body ~f:(compile_stmt env) in
    if List.is_empty orelse
    then (
      let jump_to, label = O.label () in
      List.concat [ test; [ O.jump POP_JUMP_IF_FALSE jump_to ]; body; [ label ] ])
    else (
      let orelse = List.concat_map orelse ~f:(compile_stmt env) in
      let jump_to1, label1 = O.label () in
      let jump_to2, label2 = O.label () in
      List.concat
        [ test
        ; [ O.jump POP_JUMP_IF_FALSE jump_to1 ]
        ; body
        ; [ O.jump JUMP_ABSOLUTE jump_to2; label1 ]
        ; orelse
        ; [ label2 ]
        ])
  | For _ -> failwith "Unsupported: For"
  | While { test; body; orelse } ->
    let test = compile_expr env test in
    let body = List.concat_map body ~f:(compile_stmt env) in
    let jump_to1, label1 = O.label () in
    let jump_to2, label2 = O.label () in
    let orelse = List.concat_map orelse ~f:(compile_stmt env) in
    List.concat
      [ [ label2 ]
      ; test
      ; [ O.jump POP_JUMP_IF_FALSE jump_to1 ]
      ; body
      ; [ O.jump JUMP_ABSOLUTE jump_to2; label1 ]
      ; orelse
      ]
  | Raise _ -> failwith "Unsupported: Raise"
  | Try _ -> failwith "Unsupported: Try"
  | With _ -> failwith "Unsupported: With"
  | Assert _ -> failwith "Unsupported: Assert"
  | Import _ -> failwith "Unsupported: Import"
  | ImportFrom _ -> failwith "Unsupported: ImportFrom"
  | Expr { value } -> compile_expr env value @ [ O.op POP_TOP ]
  | Assign { targets; value } -> assign env ~targets ~value
  | AugAssign { target; op; value } -> aug_assign env ~target ~op ~value
  | Return { value } ->
    let load_value =
      match value with
      | None -> [ Env.load_const env Bc_value.none ]
      | Some expr -> compile_expr env expr
    in
    load_value @ [ O.op RETURN_VALUE ]
  | Delete _ -> failwith "Unsupported: Delete"
  | Pass -> [ O.op NOP ]
  | Break -> failwith "Unsupported: Break"
  | Continue -> failwith "Unsupported: Continue"

and compile_expr env expr =
  match (expr : Ast.expr) with
  | None_ -> [ Env.load_const env Bc_value.none ]
  | Bool b -> [ Env.load_const env (Bc_value.bool b) ]
  | Num n -> [ Env.load_const env (Bc_value.int n) ]
  | Float f -> [ Env.load_const env (Bc_value.float f) ]
  | Str s -> [ Env.load_const env (Bc_value.str s) ]
  | Name name -> [ Env.load_name env name ]
  | List exprs ->
    let exprs = Array.to_list exprs in
    List.concat_map exprs ~f:(compile_expr env)
    @ [ O.op BUILD_LIST ~arg:(List.length exprs) ]
  | Dict _ -> failwith "Unsupported: Dict"
  | ListComp _ -> failwith "Unsupported: ListComp"
  | Tuple exprs ->
    let exprs = Array.to_list exprs in
    List.concat_map exprs ~f:(compile_expr env)
    @ [ O.op BUILD_TUPLE ~arg:(List.length exprs) ]
  | Lambda _ -> failwith "Unsupported: Lambda"
  | BoolOp { op = And; values } ->
    let values = List.map values ~f:(compile_expr env) in
    let jump_to, label = O.label () in
    let code =
      List.intersperse values ~sep:[ O.jump JUMP_IF_FALSE_OR_POP jump_to ] |> List.concat
    in
    code @ [ label ]
  | BoolOp { op = Or; values } ->
    let values = List.map values ~f:(compile_expr env) in
    let jump_to, label = O.label () in
    let code =
      List.intersperse values ~sep:[ O.jump JUMP_IF_TRUE_OR_POP jump_to ] |> List.concat
    in
    code @ [ label ]
  | BinOp { left; op; right } ->
    List.concat
      [ compile_expr env left; compile_expr env right; [ O.op (binop_opcode op) ] ]
  | UnaryOp { op; operand } -> compile_expr env operand @ [ O.op (unaryop_opcode op) ]
  | IfExp { test; body; orelse } ->
    let test = compile_expr env test in
    let body = compile_expr env body in
    let orelse = compile_expr env orelse in
    let jump_to1, label1 = O.label () in
    let jump_to2, label2 = O.label () in
    List.concat
      [ test
      ; [ O.jump POP_JUMP_IF_FALSE jump_to1 ]
      ; body
      ; [ O.jump JUMP_ABSOLUTE jump_to2; label1 ]
      ; orelse
      ; [ label2 ]
      ]
  | Compare { left; ops_and_exprs = [ (cmpop, expr) ] } ->
    let left = compile_expr env left in
    let expr = compile_expr env expr in
    let arg = Bc_code.int_of_cmpop cmpop in
    left @ expr @ [ O.op COMPARE_OP ~arg ]
  | Compare { left; ops_and_exprs } ->
    let left = compile_expr env left in
    let jump_to, label = O.label () in
    let nops_and_exprs = List.length ops_and_exprs in
    left
    @ List.concat_mapi ops_and_exprs ~f:(fun index (cmpop, expr) ->
          let arg = Bc_code.int_of_cmpop cmpop in
          let expr =
            compile_expr env expr @ [ O.op DUP_TOP; O.op ROT_THREE; O.op COMPARE_OP ~arg ]
          in
          let tail =
            if index = nops_and_exprs - 1
            then [ label ]
            else [ O.jump JUMP_IF_FALSE_OR_POP jump_to ]
          in
          expr @ tail)
    @ [ O.op ROT_TWO; O.op POP_TOP ]
  | Call { func; args; keywords } ->
    let _ = keywords (* TODO: handle keywords *) in
    let func = compile_expr env func in
    let args = List.map args ~f:(compile_expr env) in
    List.concat (func :: args) @ [ O.op CALL_FUNCTION ~arg:(List.length args) ]
  | Attribute { value; attr } ->
    let value = compile_expr env value in
    value @ [ Env.load_attr env attr ]
  | Subscript { value; slice } ->
    let value = compile_expr env value in
    let slice = compile_expr env slice in
    value @ slice @ [ O.op BINARY_SUBSCR ]

and aug_assign env ~target ~op ~value =
  let _ = env, target, op, value in
  failwith "Unsupported: AugAssign"

and assign env ~targets ~value =
  let value = compile_expr env value in
  let targets =
    List.concat_map targets ~f:(function
        | None_ | Bool _ | Num _ | Float _ | Str _ ->
          errorf "SyntaxError: can't assign to constant"
        | Dict _ -> errorf "SyntaxError: can't assign to dict"
        | BoolOp _ | BinOp _ | UnaryOp _ | IfExp _ | Compare _ ->
          errorf "SyntaxError: can't assign to operator"
        | Call _ -> errorf "SyntaxError: can't assign to function call"
        | ListComp _ -> errorf "SyntaxError: can't assign to comprehension"
        | Lambda _ -> errorf "SyntaxError: can't assign to lambda"
        | Name name -> [ Env.store_name env name ]
        | Tuple _exprs -> failwith "Unsupported: Assign Tuple"
        | List _exprs -> failwith "Unsupported: Assign List"
        | Attribute { value; attr } ->
          let value = compile_expr env value in
          value @ [ Env.store_attr env attr ]
        | Subscript { value; slice } ->
          let value = compile_expr env value in
          let slice = compile_expr env slice in
          value @ slice @ [ O.op STORE_SUBSCR ])
  in
  let dups = List.init (List.length targets - 1) ~f:(fun _ -> O.op DUP_TOP) in
  value @ dups @ targets

and compile (ast : Ast.t) =
  let env = Env.create () in
  let opcodes = List.concat_map ast ~f:(compile_stmt env) |> O.to_opcodes in
  { Bc_code.opcodes
  ; consts = Env.consts env
  ; varnames = Env.varnames env
  ; names = Env.names env
  }
