open Base

module Defined = struct
  type 'a t =
    | Defined of 'a
    | Undefined
end

type t =
  { stack : Bc_value.t Stack.t
  ; consts : Bc_value.t array
  ; (* TODO: use string for varnames and names and do some lookup in the related
     environments (global/local). *)
    varnames : Bc_value.t Defined.t array
  ; names : Bc_value.t Defined.t array
  }

let pop_top stack = ignore (Stack.pop_exn stack : Bc_value.t)

let rot_two stack =
  let a = Stack.pop_exn stack in
  let b = Stack.pop_exn stack in
  Stack.push stack a;
  Stack.push stack b

let rot_three stack =
  let a = Stack.pop_exn stack in
  let b = Stack.pop_exn stack in
  let c = Stack.pop_exn stack in
  Stack.push stack a;
  Stack.push stack c;
  Stack.push stack b

let dup_top stack =
  let a = Stack.top_exn stack in
  Stack.push stack a

let dup_top_two stack =
  let a = Stack.pop_exn stack in
  let b = Stack.top_exn stack in
  Stack.push stack b;
  Stack.push stack a;
  Stack.push stack a

module Unary_op = struct
  type t =
    | Positive
    | Negative
    | Not
    | Invert

  let apply t _v =
    match t with
    | Positive -> failwith "Unsupported: Positive"
    | Negative -> failwith "Unsupported: Negative"
    | Not -> failwith "Unsupported: Not"
    | Invert -> failwith "Unsupported: Invert"
end

module Binary_op = struct
  type t =
    | Matrix_multiply
    | Power
    | Multiply
    | Add
    | Subtract
    | Subscr
    | Floor_divide
    | True_divide
    | Modulo
    | Lshift
    | Rshift
    | And
    | Xor
    | Or

  let apply t _v1 _v2 =
    match t with
    | Matrix_multiply -> failwith "Unsupported: Matrix_multiply"
    | Power -> failwith "Unsupported: Power"
    | Multiply -> failwith "Unsupported: Multiply"
    | Add -> failwith "Unsupported: Add"
    | Subtract -> failwith "Unsupported: Subtract"
    | Subscr -> failwith "Unsupported: Subscr"
    | Floor_divide -> failwith "Unsupported: Floor_divide"
    | True_divide -> failwith "Unsupported: True_divide"
    | Modulo -> failwith "Unsupported: Modulo"
    | Lshift -> failwith "Unsupported: Lshift"
    | Rshift -> failwith "Unsupported: Rshift"
    | And -> failwith "Unsupported: And"
    | Xor -> failwith "Unsupported: Xor"
    | Or -> failwith "Unsupported: Or"

  let apply_inplace t _v1 _v2 =
    match t with
    | Matrix_multiply -> failwith "Unsupported: Matrix_multiply"
    | Power -> failwith "Unsupported: Power"
    | Multiply -> failwith "Unsupported: Multiply"
    | Add -> failwith "Unsupported: Add"
    | Subtract -> failwith "Unsupported: Subtract"
    | Subscr -> failwith "Unsupported: Subscr"
    | Floor_divide -> failwith "Unsupported: Floor_divide"
    | True_divide -> failwith "Unsupported: True_divide"
    | Modulo -> failwith "Unsupported: Modulo"
    | Lshift -> failwith "Unsupported: Lshift"
    | Rshift -> failwith "Unsupported: Rshift"
    | And -> failwith "Unsupported: And"
    | Xor -> failwith "Unsupported: Xor"
    | Or -> failwith "Unsupported: Or"
end

let unary op stack =
  let tos = Stack.pop_exn stack in
  let tos = Unary_op.apply op tos in
  Stack.push stack tos

let binary op stack =
  let tos = Stack.pop_exn stack in
  let tos1 = Stack.pop_exn stack in
  let tos = Binary_op.apply op tos1 tos in
  Stack.push stack tos

let inplace op stack =
  let tos = Stack.pop_exn stack in
  let tos1 = Stack.pop_exn stack in
  let tos = Binary_op.apply_inplace op tos1 tos in
  Stack.push stack tos

let load_fast t ~arg =
  match t.varnames.(arg) with
  | Defined v -> Stack.push t.stack v
  | Undefined -> failwith "local not defined"

let store_fast t ~arg =
  let tos = Stack.pop_exn t.stack in
  t.varnames.(arg) <- Defined tos

let delete_fast t ~arg = t.varnames.(arg) <- Undefined

let load_global t ~arg =
  match t.names.(arg) with
  | Defined v -> Stack.push t.stack v
  | Undefined -> failwith "global not defined"

let store_global t ~arg =
  let tos = Stack.pop_exn t.stack in
  t.names.(arg) <- Defined tos

let delete_global t ~arg = t.names.(arg) <- Undefined

let eval t opcode =
  match (opcode : Bc_opcode.t) with
  | POP_TOP -> pop_top t.stack
  | ROT_TWO -> rot_two t.stack
  | ROT_THREE -> rot_three t.stack
  | DUP_TOP -> dup_top t.stack
  | DUP_TOP_TWO -> dup_top_two t.stack
  | NOP -> ()
  | UNARY_POSITIVE -> unary Positive t.stack
  | UNARY_NEGATIVE -> unary Negative t.stack
  | UNARY_NOT -> unary Not t.stack
  | UNARY_INVERT -> unary Invert t.stack
  | BINARY_MATRIX_MULTIPLY -> binary Matrix_multiply t.stack
  | INPLACE_MATRIX_MULTIPLY -> inplace Matrix_multiply t.stack
  | BINARY_POWER -> binary Power t.stack
  | BINARY_MULTIPLY -> binary Multiply t.stack
  | BINARY_MODULO -> binary Modulo t.stack
  | BINARY_ADD -> binary Add t.stack
  | BINARY_SUBTRACT -> binary Subtract t.stack
  | BINARY_SUBSCR -> binary Subscr t.stack
  | BINARY_FLOOR_DIVIDE -> binary Floor_divide t.stack
  | BINARY_TRUE_DIVIDE -> binary True_divide t.stack
  | INPLACE_FLOOR_DIVIDE -> inplace Floor_divide t.stack
  | INPLACE_TRUE_DIVIDE -> inplace True_divide t.stack
  | GET_AITER -> failwith "Unsupported: GET_AITER"
  | GET_ANEXT -> failwith "Unsupported: GET_ANEXT"
  | BEFORE_ASYNC_WITH -> failwith "Unsupported: BEFORE_ASYNC_WITH"
  | INPLACE_ADD -> inplace Add t.stack
  | INPLACE_SUBTRACT -> inplace Subtract t.stack
  | INPLACE_MULTIPLY -> inplace Multiply t.stack
  | INPLACE_MODULO -> inplace Modulo t.stack
  | STORE_SUBSCR -> failwith "Unsupported: STORE_SUBSCR"
  | DELETE_SUBSCR -> failwith "Unsupported: DELETE_SUBSCR"
  | BINARY_LSHIFT -> binary Lshift t.stack
  | BINARY_RSHIFT -> binary Rshift t.stack
  | BINARY_AND -> binary And t.stack
  | BINARY_XOR -> binary Xor t.stack
  | BINARY_OR -> binary Or t.stack
  | INPLACE_POWER -> inplace Power t.stack
  | GET_ITER -> failwith "Unsupported: GET_ITER"
  | GET_YIELD_FROM_ITER -> failwith "Unsupported: GET_YIELD_FROM_ITER"
  | PRINT_EXPR -> failwith "Unsupported: PRINT_EXPR"
  | LOAD_BUILD_CLASS -> failwith "Unsupported: LOAD_BUILD_CLASS"
  | YIELD_FROM -> failwith "Unsupported: YIELD_FROM"
  | GET_AWAITABLE -> failwith "Unsupported: GET_AWAITABLE"
  | INPLACE_LSHIFT -> inplace Lshift t.stack
  | INPLACE_RSHIFT -> inplace Rshift t.stack
  | INPLACE_AND -> inplace And t.stack
  | INPLACE_XOR -> inplace Xor t.stack
  | INPLACE_OR -> inplace Or t.stack
  | BREAK_LOOP -> failwith "Unsupported: BREAK_LOOP"
  | WITH_CLEANUP_START -> failwith "Unsupported: WITH_CLEANUP_START"
  | WITH_CLEANUP_FINISH -> failwith "Unsupported: WITH_CLEANUP_FINISH"
  | RETURN_VALUE -> failwith "Unsupported: RETURN_VALUE"
  | IMPORT_STAR -> failwith "Unsupported: IMPORT_STAR"
  | SETUP_ANNOTATIONS -> failwith "Unsupported: SETUP_ANNOTATIONS"
  | YIELD_VALUE -> failwith "Unsupported: YIELD_VALUE"
  | POP_BLOCK -> failwith "Unsupported: POP_BLOCK"
  | END_FINALLY -> failwith "Unsupported: END_FINALLY"
  | POP_EXCEPT -> failwith "Unsupported: POP_EXCEPT"
  | STORE_NAME -> failwith "Unsupported: STORE_NAME"
  | DELETE_NAME -> failwith "Unsupported: DELETE_NAME"
  | UNPACK_SEQUENCE -> failwith "Unsupported: UNPACK_SEQUENCE"
  | FOR_ITER -> failwith "Unsupported: FOR_ITER"
  | UNPACK_EX -> failwith "Unsupported: UNPACK_EX"
  | STORE_ATTR -> failwith "Unsupported: STORE_ATTR"
  | DELETE_ATTR -> failwith "Unsupported: DELETE_ATTR"
  | STORE_GLOBAL arg -> store_global t ~arg
  | DELETE_GLOBAL arg -> delete_global t ~arg
  | LOAD_CONST arg -> Stack.push t.stack t.consts.(arg)
  | LOAD_NAME -> failwith "Unsupported: LOAD_NAME"
  | BUILD_TUPLE -> failwith "Unsupported: BUILD_TUPLE"
  | BUILD_LIST -> failwith "Unsupported: BUILD_LIST"
  | BUILD_SET -> failwith "Unsupported: BUILD_SET"
  | BUILD_MAP -> failwith "Unsupported: BUILD_MAP"
  | LOAD_ATTR -> failwith "Unsupported: LOAD_ATTR"
  | COMPARE_OP -> failwith "Unsupported: COMPARE_OP"
  | IMPORT_NAME -> failwith "Unsupported: IMPORT_NAME"
  | IMPORT_FROM -> failwith "Unsupported: IMPORT_FROM"
  | JUMP_FORWARD -> failwith "Unsupported: JUMP_FORWARD"
  | JUMP_IF_FALSE_OR_POP -> failwith "Unsupported: JUMP_IF_FALSE_OR_POP"
  | JUMP_IF_TRUE_OR_POP -> failwith "Unsupported: JUMP_IF_TRUE_OR_POP"
  | JUMP_ABSOLUTE -> failwith "Unsupported: JUMP_ABSOLUTE"
  | POP_JUMP_IF_FALSE -> failwith "Unsupported: POP_JUMP_IF_FALSE"
  | POP_JUMP_IF_TRUE -> failwith "Unsupported: POP_JUMP_IF_TRUE"
  | LOAD_GLOBAL arg -> load_global t ~arg
  | CONTINUE_LOOP -> failwith "Unsupported: CONTINUE_LOOP"
  | SETUP_LOOP -> failwith "Unsupported: SETUP_LOOP"
  | SETUP_EXCEPT -> failwith "Unsupported: SETUP_EXCEPT"
  | SETUP_FINALLY -> failwith "Unsupported: SETUP_FINALLY"
  | LOAD_FAST arg -> load_fast t ~arg
  | STORE_FAST arg -> store_fast t ~arg
  | DELETE_FAST arg -> delete_fast t ~arg
  | RAISE_VARARGS -> failwith "Unsupported: RAISE_VARARGS"
  | CALL_FUNCTION -> failwith "Unsupported: CALL_FUNCTION"
  | MAKE_FUNCTION -> failwith "Unsupported: MAKE_FUNCTION"
  | BUILD_SLICE -> failwith "Unsupported: BUILD_SLICE"
  | LOAD_CLOSURE -> failwith "Unsupported: LOAD_CLOSURE"
  | LOAD_DEREF -> failwith "Unsupported: LOAD_DEREF"
  | STORE_DEREF -> failwith "Unsupported: STORE_DEREF"
  | DELETE_DEREF -> failwith "Unsupported: DELETE_DEREF"
  | CALL_FUNCTION_KW -> failwith "Unsupported: CALL_FUNCTION_KW"
  | CALL_FUNCTION_EX -> failwith "Unsupported: CALL_FUNCTION_EX"
  | SETUP_WITH -> failwith "Unsupported: SETUP_WITH"
  | EXTENDED_ARG -> failwith "Unsupported: EXTENDED_ARG"
  | LIST_APPEND -> failwith "Unsupported: LIST_APPEND"
  | SET_ADD -> failwith "Unsupported: SET_ADD"
  | MAP_ADD -> failwith "Unsupported: MAP_ADD"
  | LOAD_CLASSDEREF -> failwith "Unsupported: LOAD_CLASSDEREF"
  | BUILD_LIST_UNPACK -> failwith "Unsupported: BUILD_LIST_UNPACK"
  | BUILD_MAP_UNPACK -> failwith "Unsupported: BUILD_MAP_UNPACK"
  | BUILD_MAP_UNPACK_WITH_CALL -> failwith "Unsupported: BUILD_MAP_UNPACK_WITH_CALL"
  | BUILD_TUPLE_UNPACK -> failwith "Unsupported: BUILD_TUPLE_UNPACK"
  | BUILD_SET_UNPACK -> failwith "Unsupported: BUILD_SET_UNPACK"
  | SETUP_ASYNC_WITH -> failwith "Unsupported: SETUP_ASYNC_WITH"
  | FORMAT_VALUE -> failwith "Unsupported: FORMAT_VALUE"
  | BUILD_CONST_KEY_MAP -> failwith "Unsupported: BUILD_CONST_KEY_MAP"
  | BUILD_STRING -> failwith "Unsupported: BUILD_STRING"
  | BUILD_TUPLE_UNPACK_WITH_CALL -> failwith "Unsupported: BUILD_TUPLE_UNPACK_WITH_CALL"
  | LOAD_METHOD -> failwith "Unsupported: LOAD_METHOD"
  | CALL_METHOD -> failwith "Unsupported: CALL_METHOD"
