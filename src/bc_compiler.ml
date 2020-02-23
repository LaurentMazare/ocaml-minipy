open! Base
open! Import

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
    Bc_code.Opcode.LOAD_CONST, `arg id

  let load_name t name =
    (* TODO: use varnames for local variables *)
    let id = Id_set.find_or_add t.names name in
    Bc_code.Opcode.LOAD_NAME, `arg id

  let store_name t name =
    (* TODO: use varnames for local variables *)
    let id = Id_set.find_or_add t.names name in
    Bc_code.Opcode.STORE_NAME, `arg id
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
    ; MAKE_FUNCTION, `no_arg
    ; Env.store_name env name
    ]
  | ClassDef _ -> failwith "Unsupported: ClassDef"
  | If _ -> failwith "Unsupported: If"
  | For _ -> failwith "Unsupported: For"
  | While _ -> failwith "Unsupported: While"
  | Raise _ -> failwith "Unsupported: Raise"
  | Try _ -> failwith "Unsupported: Try"
  | With _ -> failwith "Unsupported: With"
  | Assert _ -> failwith "Unsupported: Assert"
  | Import _ -> failwith "Unsupported: Import"
  | ImportFrom _ -> failwith "Unsupported: ImportFrom"
  | Expr { value } -> compile_expr env value @ [ Bc_code.Opcode.POP_TOP, `no_arg ]
  | Assign _ -> failwith "Unsupported: Assign"
  | AugAssign _ -> failwith "Unsupported: AugAssign"
  | Return { value } ->
    let load_value =
      match value with
      | None -> [ Env.load_const env Bc_value.none ]
      | Some expr -> compile_expr env expr
    in
    load_value @ [ Bc_code.Opcode.RETURN_VALUE, `no_arg ]
  | Delete _ -> failwith "Unsupported: Delete"
  | Pass -> [ Bc_code.Opcode.NOP, `no_arg ]
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
    @ [ Bc_code.Opcode.BUILD_LIST, `arg (List.length exprs) ]
  | Dict _ -> failwith "Unsupported: Dict"
  | ListComp _ -> failwith "Unsupported: ListComp"
  | Tuple exprs ->
    let exprs = Array.to_list exprs in
    List.concat_map exprs ~f:(compile_expr env)
    @ [ Bc_code.Opcode.BUILD_TUPLE, `arg (List.length exprs) ]
  | Lambda _ -> failwith "Unsupported: Lambda"
  | BoolOp { op = _; values } ->
    let _values = List.map values ~f:(compile_expr env) in
    failwith "Unsupported: BoolOp"
  | BinOp { left; op; right } ->
    List.concat
      [ compile_expr env left; compile_expr env right; [ binop_opcode op, `no_arg ] ]
  | UnaryOp { op; operand } -> compile_expr env operand @ [ unaryop_opcode op, `no_arg ]
  | IfExp { test; body; orelse } ->
    let test = compile_expr env test in
    let body = compile_expr env body in
    let orelse = compile_expr env orelse in
    let orelse_len = List.length orelse in
    let body_len = List.length body in
    test
    @ [ Bc_code.Opcode.POP_JUMP_IF_FALSE, `rel_position (2 + body_len) ]
    @ body
    @ [ Bc_code.Opcode.JUMP_ABSOLUTE, `rel_position (1 + orelse_len) ]
    @ orelse
  | Compare _ -> failwith "Unsupported: Compare"
  | Call { func; args; keywords } ->
    let _ = keywords (* TODO: handle keywords *) in
    let func = compile_expr env func in
    let args = List.map args ~f:(compile_expr env) in
    List.concat (func :: args) @ [ CALL_FUNCTION, `arg (List.length args) ]
  | Attribute _ -> failwith "Unsupported: Attribute"
  | Subscript _ -> failwith "Unsupported: Subscript"

and compile (ast : Ast.t) =
  let env = Env.create () in
  let opcodes =
    List.concat_map ast ~f:(compile_stmt env)
    |> Array.of_list_mapi ~f:(fun index (opcode, arg) ->
           let arg =
             match arg with
             | `no_arg -> 0
             | `arg v -> v
             | `rel_position offset -> index + offset
           in
           opcode, arg)
  in
  { Bc_code.opcodes
  ; consts = Env.consts env
  ; varnames = Env.varnames env
  ; names = Env.names env
  }
