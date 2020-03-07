open Base
open Import

type t =
  { stack : Bc_value.t Stack.t
  ; code : Bc_value.code
  ; mutable counter : int
  ; local_scope : Bc_scope.t
  ; global_scope : Bc_scope.t
  ; builtins : Bc_scope.t
  ; parent_frame : t option
  }

let create ~code ~local_scope ~global_scope ~builtins ~parent_frame =
  { stack = Stack.create ()
  ; code
  ; counter = 0
  ; local_scope
  ; global_scope
  ; builtins
  ; parent_frame
  }

let eval_frame = ref None
let set_eval_frame fn = eval_frame := Some fn

let top_frame ~code ~global_scope ~builtins =
  create
    ~code
    ~local_scope:(Bc_scope.create ())
    ~global_scope
    ~builtins
    ~parent_frame:None

let call_frame t ~code ~local_scope =
  create
    ~code
    ~local_scope
    ~global_scope:t.global_scope
    ~builtins:t.builtins
    ~parent_frame:(Some t)

let eval_code t ~code ~local_scope =
  let top_stack_len = Stack.length t.stack in
  let frame = call_frame t ~code ~local_scope in
  (Option.value_exn !eval_frame) ~frame;
  if Stack.length t.stack = top_stack_len
  then Bc_value.none
  else if Stack.length t.stack = top_stack_len + 1
  then Stack.pop_exn t.stack
  else
    errorf
      "unexpected stack size: initial %d, current %d"
      top_stack_len
      (Stack.length t.stack)

type internal_action =
  | Continue (* Different from loop continue. *)
  | Jump_rel of int
  | Jump_abs of int
  | Call_fn of
      { code : Bc_value.code
      ; local_scope : Bc_scope.t
      }
  | Return of Bc_value.t

let push_and_continue stack v =
  Stack.push stack v;
  Continue

let pop_top stack =
  ignore (Stack.pop_exn stack : Bc_value.t);
  Continue

let pop2 stack =
  let a = Stack.pop_exn stack in
  let b = Stack.pop_exn stack in
  b, a

let pop3 stack =
  let a = Stack.pop_exn stack in
  let b = Stack.pop_exn stack in
  let c = Stack.pop_exn stack in
  c, b, a

let rot_two stack =
  let a, b = pop2 stack in
  Stack.push stack b;
  push_and_continue stack a

let rot_three stack =
  let a, b, c = pop3 stack in
  Stack.push stack c;
  Stack.push stack a;
  push_and_continue stack b

let dup_top stack =
  let a = Stack.top_exn stack in
  push_and_continue stack a

let dup_top_two stack =
  let a, b = pop2 stack in
  Stack.push stack a;
  Stack.push stack b;
  Stack.push stack a;
  push_and_continue stack b

module Unary_op = struct
  type t =
    | Positive
    | Negative
    | Not
    | Invert
  [@@deriving sexp]

  let apply t v =
    match t, (v : Bc_value.t) with
    | Positive, ((Int _ | Float _) as v) -> v
    | Negative, Int v -> Bc_value.int (Z.neg v)
    | Negative, Float v -> Bc_value.Float (-.v)
    | Not, v -> Bc_value.bool (not (Bc_value.to_bool v))
    | _ ->
      errorf
        "TypeError: bad operand type for unary %s: %s"
        (sexp_of_t t |> Sexp.to_string)
        (Bc_value.type_as_string v)
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

  let bin_add v1 v2 =
    match (v1 : Bc_value.t), (v2 : Bc_value.t) with
    | Int v1, Int v2 -> Bc_value.int (Z.add v1 v2)
    | Float v1, Int v2 -> Bc_value.float (v1 +. Z.to_float v2)
    | Int v1, Float v2 -> Bc_value.float (Z.to_float v1 +. v2)
    | Float v1, Float v2 -> Bc_value.float (v1 +. v2)
    | Str v1, Str v2 -> Bc_value.str (v1 ^ v2)
    | Tuple v1, Tuple v2 -> Bc_value.tuple (Array.append v1 v2)
    | List v1, List v2 ->
      Array.concat [ Queue.to_array v1; Queue.to_array v2 ]
      |> Queue.of_array
      |> Bc_value.list
    | _, _ ->
      errorf
        "TypeError in add %s %s"
        (Bc_value.type_as_string v1)
        (Bc_value.type_as_string v2)

  let bin_sub v1 v2 =
    match (v1 : Bc_value.t), (v2 : Bc_value.t) with
    | Int v1, Int v2 -> Bc_value.int (Z.sub v1 v2)
    | Float v1, Int v2 -> Bc_value.float (v1 -. Z.to_float v2)
    | Int v1, Float v2 -> Bc_value.float (Z.to_float v1 -. v2)
    | Float v1, Float v2 -> Bc_value.float (v1 -. v2)
    | _, _ ->
      errorf
        "TypeError in sub %s %s"
        (Bc_value.type_as_string v1)
        (Bc_value.type_as_string v2)

  let bin_mult v1 v2 =
    match (v1 : Bc_value.t), (v2 : Bc_value.t) with
    | Int v1, Int v2 -> Bc_value.int (Z.mul v1 v2)
    | Float v1, Int v2 -> Bc_value.float (v1 *. Z.to_float v2)
    | Int v1, Float v2 -> Bc_value.float (Z.to_float v1 *. v2)
    | Float v1, Float v2 -> Bc_value.float (v1 *. v2)
    | Tuple v1, Int v2 ->
      List.init (Z.to_int v2) ~f:(fun _index -> v1) |> Array.concat |> Bc_value.tuple
    | List v1, Int v2 ->
      let q = Queue.create () in
      for _i = 1 to Z.to_int v2 do
        for i = 0 to Queue.length v1 - 1 do
          Queue.enqueue q (Queue.get v1 i)
        done
      done;
      Bc_value.list q
    | _, _ ->
      errorf
        "TypeError in mult %s %s"
        (Bc_value.type_as_string v1)
        (Bc_value.type_as_string v2)

  let bin_div v1 v2 =
    match (v1 : Bc_value.t), (v2 : Bc_value.t) with
    | Int v1, Int v2 -> Bc_value.float (Z.to_float v1 /. Z.to_float v2)
    | Float v1, Int v2 -> Bc_value.float (v1 /. Z.to_float v2)
    | Int v1, Float v2 -> Bc_value.float (Z.to_float v1 /. v2)
    | Float v1, Float v2 -> Bc_value.float (v1 /. v2)
    | _, _ ->
      errorf
        "TypeError in div %s %s"
        (Bc_value.type_as_string v1)
        (Bc_value.type_as_string v2)

  let bin_floor_div v1 v2 =
    match (v1 : Bc_value.t), (v2 : Bc_value.t) with
    | Int v1, Int v2 -> Bc_value.int (Z.div v1 v2)
    | _, _ ->
      errorf
        "TypeError in floor-div %s %s"
        (Bc_value.type_as_string v1)
        (Bc_value.type_as_string v2)

  let bin_mod v1 v2 =
    match (v1 : Bc_value.t), (v2 : Bc_value.t) with
    | Int v1, Int v2 -> Bc_value.int (Z.( mod ) v1 v2)
    | _, _ ->
      errorf
        "TypeError in mod %s %s"
        (Bc_value.type_as_string v1)
        (Bc_value.type_as_string v2)

  let bin_pow v1 v2 =
    match (v1 : Bc_value.t), (v2 : Bc_value.t) with
    | Int v1, Int v2 ->
      if Z.(equal v2 zero)
      then Bc_value.int Z.one
      else if Z.(equal v1 zero)
      then Bc_value.int Z.zero
      else if Z.(geq v2 zero)
      then Bc_value.int (Z.pow v1 (Z.to_int v2))
      else Bc_value.float (Z.to_float v1 **. Z.to_float v2)
    | Float v1, Int v2 -> Bc_value.float (v1 **. Z.to_float v2)
    | Int v1, Float v2 -> Bc_value.float (Z.to_float v1 **. v2)
    | Float v1, Float v2 -> Bc_value.float (v1 **. v2)
    | _, _ ->
      errorf
        "TypeError in pow %s %s"
        (Bc_value.type_as_string v1)
        (Bc_value.type_as_string v2)

  let bin_subscr value index =
    match (value : Bc_value.t), (index : Bc_value.t) with
    | Str s, Int index ->
      let index = Z.to_int index in
      let index = if index < 0 then String.length s + index else index in
      Bc_value.str (String.of_char s.[index])
    | Tuple vs, Int index ->
      let index = Z.to_int index in
      let index = if index < 0 then Array.length vs + index else index in
      vs.(index)
    | List vs, Int index ->
      let index = Z.to_int index in
      let index = if index < 0 then Queue.length vs + index else index in
      Queue.get vs index
    | Dict d, index ->
      (match Hashtbl.find d index with
      | Some v -> v
      | None -> errorf "KeyError")
    | _, _ ->
      errorf
        "TypeError in subscript %s %s"
        (Bc_value.type_as_string value)
        (Bc_value.type_as_string index)

  let apply t v1 v2 =
    match t with
    | Matrix_multiply -> failwith "Unsupported: Matrix_multiply"
    | Power -> bin_pow v1 v2
    | Multiply -> bin_mult v1 v2
    | Add -> bin_add v1 v2
    | Subtract -> bin_sub v1 v2
    | Subscr -> bin_subscr v1 v2
    | Floor_divide -> bin_floor_div v1 v2
    | True_divide -> bin_div v1 v2
    | Modulo -> bin_mod v1 v2
    | Lshift -> failwith "Unsupported: Lshift"
    | Rshift -> failwith "Unsupported: Rshift"
    | And -> failwith "Unsupported: And"
    | Xor -> failwith "Unsupported: Xor"
    | Or -> failwith "Unsupported: Or"

  (* inplace operators are identical to the normal operators as
     they don't handle storing the result.
     The only difference is e.g. calling __iadd__ instead of __add__. *)
  let apply_inplace t v1 v2 =
    match t with
    | Matrix_multiply -> failwith "Unsupported: Matrix_multiply"
    | Power -> bin_pow v1 v2
    | Multiply -> bin_mult v1 v2
    | Add -> bin_add v1 v2
    | Subtract -> bin_sub v1 v2
    | Subscr -> bin_subscr v1 v2
    | Floor_divide -> bin_floor_div v1 v2
    | True_divide -> bin_div v1 v2
    | Modulo -> bin_mod v1 v2
    | Lshift -> failwith "Unsupported: Lshift"
    | Rshift -> failwith "Unsupported: Rshift"
    | And -> failwith "Unsupported: And"
    | Xor -> failwith "Unsupported: Xor"
    | Or -> failwith "Unsupported: Or"
end

let unary op stack =
  let tos = Stack.pop_exn stack in
  let tos = Unary_op.apply op tos in
  push_and_continue stack tos

let binary op stack =
  let tos, tos1 = pop2 stack in
  let tos = Binary_op.apply op tos tos1 in
  push_and_continue stack tos

let inplace op stack =
  let tos, tos1 = pop2 stack in
  let tos = Binary_op.apply_inplace op tos tos1 in
  push_and_continue stack tos

let compare op stack =
  let tos, tos1 = pop2 stack in
  let tos =
    match (op : Ast.cmpop), (tos : Bc_value.t), (tos1 : Bc_value.t) with
    | Lt, Int v1, Int v2 -> Bc_value.bool (Z.lt v1 v2)
    | Lt, Float v1, Int v2 -> Bc_value.bool (Float.( < ) v1 (Z.to_float v2))
    | Lt, Int v1, Float v2 -> Bc_value.bool (Float.( < ) (Z.to_float v1) v2)
    | Lt, Float v1, Float v2 -> Bc_value.bool (Float.( < ) v1 v2)
    | LtE, Int v1, Int v2 -> Bc_value.bool (Z.leq v1 v2)
    | LtE, Float v1, Int v2 -> Bc_value.bool (Float.( <= ) v1 (Z.to_float v2))
    | LtE, Int v1, Float v2 -> Bc_value.bool (Float.( <= ) (Z.to_float v1) v2)
    | LtE, Float v1, Float v2 -> Bc_value.bool (Float.( <= ) v1 v2)
    | Gt, Int v1, Int v2 -> Bc_value.bool (Z.gt v1 v2)
    | Gt, Float v1, Int v2 -> Bc_value.bool (Float.( > ) v1 (Z.to_float v2))
    | Gt, Int v1, Float v2 -> Bc_value.bool (Float.( > ) (Z.to_float v1) v2)
    | Gt, Float v1, Float v2 -> Bc_value.bool (Float.( > ) v1 v2)
    | GtE, Int v1, Int v2 -> Bc_value.bool (Z.geq v1 v2)
    | GtE, Float v1, Int v2 -> Bc_value.bool (Float.( >= ) v1 (Z.to_float v2))
    | GtE, Int v1, Float v2 -> Bc_value.bool (Float.( >= ) (Z.to_float v1) v2)
    | GtE, Float v1, Float v2 -> Bc_value.bool (Float.( >= ) v1 v2)
    | Eq, Int v1, Int v2 -> Bc_value.bool (Z.equal v1 v2)
    | Eq, Float v1, Int v2 -> Bc_value.bool (Float.( = ) v1 (Z.to_float v2))
    | Eq, Int v1, Float v2 -> Bc_value.bool (Float.( = ) (Z.to_float v1) v2)
    | Eq, Float v1, Float v2 -> Bc_value.bool (Float.( = ) v1 v2)
    | NotEq, Int v1, Int v2 -> Bc_value.bool (not (Z.equal v1 v2))
    | NotEq, Float v1, Int v2 -> Bc_value.bool (Float.( <> ) v1 (Z.to_float v2))
    | NotEq, Int v1, Float v2 -> Bc_value.bool (Float.( <> ) (Z.to_float v1) v2)
    | NotEq, Float v1, Float v2 -> Bc_value.bool (Float.( <> ) v1 v2)
    | _, _, _ ->
      errorf
        "TypeError in %s %s %s"
        (Ast.sexp_of_cmpop op |> Sexp.to_string)
        (Bc_value.type_as_string tos)
        (Bc_value.type_as_string tos1)
  in
  push_and_continue stack tos

let load_fast t ~arg =
  let name = t.code.varnames.(arg) in
  match Bc_scope.find t.local_scope t.code.varnames.(arg) with
  | Some v -> push_and_continue t.stack v
  | None -> errorf "local %s is not defined" name

let store_fast t ~arg =
  let tos = Stack.pop_exn t.stack in
  Bc_scope.set t.local_scope t.code.varnames.(arg) tos;
  Continue

let delete_fast t ~arg =
  Bc_scope.remove t.local_scope t.code.varnames.(arg);
  Continue

let load_global t ~arg =
  let name = t.code.names.(arg) in
  match Bc_scope.find t.global_scope name with
  | Some v -> push_and_continue t.stack v
  | None -> errorf "global %s is not defined" name

let store_global t ~arg =
  let tos = Stack.pop_exn t.stack in
  Bc_scope.set t.global_scope t.code.names.(arg) tos;
  Continue

let delete_global t ~arg =
  Bc_scope.remove t.global_scope t.code.names.(arg);
  Continue

let find_name t name =
  let rec loop_locals t =
    match Bc_scope.find t.local_scope name with
    | Some _ as some -> some
    | None ->
      (match t.parent_frame with
      | Some parent_frame -> loop_locals parent_frame
      | None -> None)
  in
  match loop_locals t with
  | Some _ as v -> v
  | None ->
    (match Bc_scope.find t.global_scope name with
    | Some _ as v -> v
    | None -> Bc_scope.find t.builtins name)

let load_name t ~arg =
  let name = t.code.names.(arg) in
  match find_name t name with
  | None -> errorf "NameError: name '%s' is not defined" name
  | Some value -> push_and_continue t.stack value

let unpack_sequence stack ~arg =
  let value = Stack.pop_exn stack in
  (match (value : Bc_value.t) with
  | List ts ->
    if Queue.length ts <> arg
    then
      errorf
        "TypeError when unpacking list of length %d in %d items"
        (Queue.length ts)
        arg;
    for i = Queue.length ts - 1 downto 0 do
      Stack.push stack (Queue.get ts i)
    done
  | Tuple ts ->
    if Array.length ts <> arg
    then
      errorf
        "TypeError when unpacking tuple of length %d in %d items"
        (Array.length ts)
        arg;
    for i = Array.length ts - 1 downto 0 do
      Stack.push stack ts.(i)
    done
  | _ -> errorf "TypeError when unpacking sequence %s" (Bc_value.type_as_string value));
  Continue

let store_subscr stack =
  let value, obj, index = pop3 stack in
  (match (obj : Bc_value.t), (index : Bc_value.t) with
  | List ts, Int i ->
    let i = Z.to_int i in
    let i = if i < 0 then Queue.length ts + i else i in
    Queue.set ts i value
  | Dict d, index -> Hashtbl.set d ~key:index ~data:value
  | _, _ ->
    errorf
      "TypeError in store subscript %s[%s]"
      (Bc_value.type_as_string obj)
      (Bc_value.type_as_string index));
  Continue

let delete_subscr stack =
  let obj, index = pop2 stack in
  (match (obj : Bc_value.t), (index : Bc_value.t) with
  | List _ts, Int _i -> failwith "cannot delete from list for now"
  | Dict d, index -> Hashtbl.remove d index
  | _, _ ->
    errorf
      "TypeError in store subscript %s[%s]"
      (Bc_value.type_as_string obj)
      (Bc_value.type_as_string index));
  Continue

let popn stack n =
  let rec loop acc n =
    match n with
    | 0 -> acc
    | n -> loop (Stack.pop_exn stack :: acc) (n - 1)
  in
  loop [] n

let build_tuple t ~arg =
  let tuple = popn t.stack arg |> Array.of_list in
  push_and_continue t.stack (Tuple tuple)

let build_list t ~arg =
  let list = popn t.stack arg |> Queue.of_list in
  push_and_continue t.stack (List list)

let get_iter stack =
  let iter = Stack.pop_exn stack |> Bc_value.to_iterable in
  push_and_continue stack iter

let for_iter stack ~arg =
  match Stack.top_exn stack with
  | Bc_value.Iterator { next } ->
    (match next () with
    | Some v -> push_and_continue stack v
    | None ->
      let _top = Stack.pop_exn stack in
      Jump_abs arg)
  | v -> Bc_value.cannot_be_interpreted_as v "iterator"

let list_append stack ~arg =
  let v = Stack.pop_exn stack in
  let list =
    Stack.fold_until
      stack
      ~init:1
      ~f:(fun i value -> if i = arg then Stop value else Continue (i + 1))
      ~finish:(fun _ -> errorf "LIST_APPEND cannot find list")
  in
  match list with
  | Bc_value.List l ->
    Queue.enqueue l v;
    Continue
  | v -> Bc_value.cannot_be_interpreted_as v "list"

let build_map stack =
  let tbl = Hashtbl.Poly.create () in
  push_and_continue stack (Bc_value.Dict tbl)

let load_attr stack attr =
  match (Stack.pop_exn stack : Bc_value.t) with
  | List q ->
    let attr = Bc_list.attrs q ~attr in
    push_and_continue stack attr
  | Object { cls; attrs } ->
    (match Hashtbl.find attrs attr with
    | Some v -> push_and_continue stack v
    | None -> errorf "object from class '%s' has no attribute '%s'" cls.cls_name attr)
  | v -> errorf "'%s' object has no attribute '%s'" (Bc_value.type_as_string v) attr

let call_scope (args : Ast.arguments) ~defaults ~arg_values ~keyword_values =
  let scope = Bc_scope.create () in
  let rec loop arg_and_expr =
    match arg_and_expr with
    | [], [] | _ :: _, [] | [], _ :: _ -> arg_and_expr
    | name :: args, value :: arg_values ->
      Bc_scope.set scope name value;
      loop (args, arg_values)
  in
  let pos_args, pos_exprs = loop (args.args, arg_values) in
  List.iter pos_args ~f:(fun name ->
      match Hashtbl.find_and_remove keyword_values name with
      | None ->
        errorf
          "function takes %d positional arguments but was given %d"
          (List.length args.args)
          (List.length arg_values)
      | Some value -> Bc_scope.set scope name value);
  (match args.vararg with
  | Some name -> Bc_scope.set scope name (Bc_value.list (Queue.of_list pos_exprs))
  | None ->
    if not (List.is_empty pos_exprs)
    then
      errorf
        "function takes %d positional arguments but was given %d"
        (List.length args.args)
        (List.length arg_values));
  List.iter defaults ~f:(fun (name, default) ->
      let value = Hashtbl.find_and_remove keyword_values name |> Option.value ~default in
      Bc_scope.set scope name value);
  (match args.kwarg with
  | Some name ->
    let dict =
      Hashtbl.to_alist keyword_values
      |> List.map ~f:(fun (name, value) -> Bc_value.str name, value)
      |> Hashtbl.Poly.of_alist_exn
    in
    Bc_scope.set scope name (Bc_value.Dict dict)
  | None ->
    if not (Hashtbl.is_empty keyword_values)
    then
      errorf
        "function received too many keyword arguments %s"
        (Hashtbl.keys keyword_values |> String.concat ~sep:","));
  scope

let eval_fn t fn arg_values keyword_values =
  let { Bc_value.name = _; code; args; defaults; captured; method_self } = fn in
  let arg_values =
    match method_self with
    | None -> arg_values
    | Some self -> self :: arg_values
  in
  let keyword_values = Hashtbl.of_alist_exn (module String) keyword_values in
  let local_scope = call_scope args ~defaults ~arg_values ~keyword_values in
  List.iter captured ~f:(fun (name, value) ->
      if not (Bc_scope.mem local_scope name) then Bc_scope.set local_scope name value);
  eval_code t ~code ~local_scope

let call_function_aux t ~kwarg_names ~arg =
  let n_kwargs = Array.length kwarg_names in
  let kwargs = popn t.stack n_kwargs in
  let n_posargs = arg - n_kwargs in
  let arg_values = popn t.stack n_posargs in
  let fn = Stack.pop_exn t.stack in
  let keyword_values = List.zip_exn (Array.to_list kwarg_names) kwargs in
  match (fn : Bc_value.t) with
  | Builtin_fn { name = _; fn } ->
    let keyword_values = Map.of_alist_exn (module String) keyword_values in
    let value = fn (eval_fn t) arg_values keyword_values in
    push_and_continue t.stack value
  | Function { name = _; code; args; defaults; captured; method_self } ->
    let keyword_values = Hashtbl.of_alist_exn (module String) keyword_values in
    let arg_values =
      match method_self with
      | None -> arg_values
      | Some self -> self :: arg_values
    in
    let local_scope = call_scope args ~defaults ~arg_values ~keyword_values in
    List.iter captured ~f:(fun (name, value) ->
        if not (Bc_scope.mem local_scope name) then Bc_scope.set local_scope name value);
    Call_fn { code; local_scope }
  | Class ({ attrs; _ } as cls) ->
    let self_attrs = Hashtbl.create (module String) in
    let self = Bc_value.Object { cls; attrs = self_attrs } in
    Hashtbl.iteri attrs ~f:(fun ~key ~data ->
        let data =
          match data with
          | Function fn -> Bc_value.Function { fn with method_self = Some self }
          | data -> data
        in
        Hashtbl.set self_attrs ~key ~data);
    (match Hashtbl.find self_attrs "__init__" with
    | None -> ()
    | Some (Function fn) -> ignore (eval_fn t fn arg_values keyword_values : Bc_value.t)
    | Some v -> Bc_value.cannot_be_interpreted_as v "callable");
    push_and_continue t.stack self
  | _ -> errorf "'%s' object is not callable" (Bc_value.type_as_string fn)

let call_function_kw t ~arg =
  let kwarg_names =
    match (Stack.pop_exn t.stack : Bc_value.t) with
    | Tuple tuple ->
      Array.map tuple ~f:(function
          | Str name -> name
          | _ -> errorf "CALL_FUNCTION_KW expects a tuple of string")
    | v -> errorf "CALL_FUNCTION_KW expects a tuple, got '%s'" (Bc_value.type_as_string v)
  in
  call_function_aux t ~arg ~kwarg_names

let call_function t ~arg = call_function_aux t ~kwarg_names:[||] ~arg

let build_class t =
  let fn _eval_fn args _kwargs =
    let code, parent_class, cls_name =
      match (args : Bc_value.t list) with
      | [ Code { code; _ }; Str name ] -> code, None, name
      | [ Code { code; _ }; Str name; Class parent_class ] ->
        code, Some parent_class, name
      | _ ->
        errorf
          "build_class expects either two arguments (code, name) or three arguments \
           (code, class, name), got (%s)"
          (List.map args ~f:Bc_value.type_as_string |> String.concat ~sep:", ")
    in
    let local_scope = Bc_scope.create () in
    ignore (eval_code t ~code ~local_scope : Bc_value.t);
    let attrs = Bc_scope.to_attrs local_scope in
    (match parent_class with
    | None -> ()
    | Some parent_class ->
      Hashtbl.iteri parent_class.attrs ~f:(fun ~key ~data ->
          if not (Hashtbl.mem attrs key) then Hashtbl.set attrs ~key ~data));
    Bc_value.Class { cls_name; attrs; parent_class; id = Bc_value.Class_id.create () }
  in
  Bc_value.Builtin_fn { name = "build_class"; fn }

let store_attr t ~arg =
  let name = t.code.names.(arg) in
  let tos1, tos = pop2 t.stack in
  match tos with
  | Object { attrs; _ } ->
    Hashtbl.set attrs ~key:name ~data:tos1;
    Continue
  | tos -> errorf "cannot set attribute on '%s'" (Bc_value.type_as_string tos)

let delete_attr t ~arg =
  let name = t.code.names.(arg) in
  match Stack.pop_exn t.stack with
  | Object { attrs; _ } ->
    Hashtbl.remove attrs name;
    Continue
  | tos -> errorf "cannot delete attribute on '%s'" (Bc_value.type_as_string tos)

let eval_one t opcode ~arg =
  match (opcode : Bc_code.Opcode.t) with
  | POP_TOP -> pop_top t.stack
  | ROT_TWO -> rot_two t.stack
  | ROT_THREE -> rot_three t.stack
  | DUP_TOP -> dup_top t.stack
  | DUP_TOP_TWO -> dup_top_two t.stack
  | NOP -> Continue
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
  | STORE_SUBSCR -> store_subscr t.stack
  | DELETE_SUBSCR -> delete_subscr t.stack
  | BINARY_LSHIFT -> binary Lshift t.stack
  | BINARY_RSHIFT -> binary Rshift t.stack
  | BINARY_AND -> binary And t.stack
  | BINARY_XOR -> binary Xor t.stack
  | BINARY_OR -> binary Or t.stack
  | INPLACE_POWER -> inplace Power t.stack
  | GET_ITER -> get_iter t.stack
  | GET_YIELD_FROM_ITER -> failwith "Unsupported: GET_YIELD_FROM_ITER"
  | PRINT_EXPR -> failwith "Unsupported: PRINT_EXPR"
  | LOAD_BUILD_CLASS -> push_and_continue t.stack (build_class t)
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
  | RETURN_VALUE -> Return (Stack.pop_exn t.stack)
  | IMPORT_STAR -> failwith "Unsupported: IMPORT_STAR"
  | SETUP_ANNOTATIONS -> failwith "Unsupported: SETUP_ANNOTATIONS"
  | YIELD_VALUE -> failwith "Unsupported: YIELD_VALUE"
  | POP_BLOCK -> failwith "Unsupported: POP_BLOCK"
  | END_FINALLY -> failwith "Unsupported: END_FINALLY"
  | POP_EXCEPT -> failwith "Unsupported: POP_EXCEPT"
  | STORE_NAME ->
    let name = t.code.names.(arg) in
    let value = Stack.pop_exn t.stack in
    Bc_scope.set t.local_scope name value;
    Continue
  | DELETE_NAME ->
    let name = t.code.names.(arg) in
    Bc_scope.remove t.local_scope name;
    Continue
  | UNPACK_SEQUENCE -> unpack_sequence t.stack ~arg
  | FOR_ITER -> for_iter t.stack ~arg
  | UNPACK_EX -> failwith "Unsupported: UNPACK_EX"
  | STORE_ATTR -> store_attr t ~arg
  | DELETE_ATTR -> delete_attr t ~arg
  | STORE_GLOBAL -> store_global t ~arg
  | DELETE_GLOBAL -> delete_global t ~arg
  | LOAD_CONST -> push_and_continue t.stack t.code.consts.(arg)
  | LOAD_NAME -> load_name t ~arg
  | BUILD_TUPLE -> build_tuple t ~arg
  | BUILD_LIST -> build_list t ~arg
  | BUILD_SET -> failwith "Unsupported: BUILD_SET"
  | BUILD_MAP -> build_map t.stack
  | LOAD_ATTR -> load_attr t.stack t.code.names.(arg)
  | COMPARE_OP -> compare (Bc_code.cmpop_of_int arg) t.stack
  | IMPORT_NAME -> failwith "Unsupported: IMPORT_NAME"
  | IMPORT_FROM -> failwith "Unsupported: IMPORT_FROM"
  | JUMP_FORWARD -> Jump_rel arg
  | JUMP_IF_FALSE_OR_POP ->
    let v = Stack.top_exn t.stack in
    if not (Bc_value.to_bool v)
    then Jump_abs arg
    else (
      ignore (Stack.pop_exn t.stack : Bc_value.t);
      Continue)
  | JUMP_IF_TRUE_OR_POP ->
    let v = Stack.top_exn t.stack in
    if Bc_value.to_bool v
    then Jump_abs arg
    else (
      ignore (Stack.pop_exn t.stack : Bc_value.t);
      Continue)
  | JUMP_ABSOLUTE -> Jump_abs arg
  | POP_JUMP_IF_FALSE ->
    let v = Stack.pop_exn t.stack in
    if not (Bc_value.to_bool v) then Jump_abs arg else Continue
  | POP_JUMP_IF_TRUE ->
    let v = Stack.pop_exn t.stack in
    if Bc_value.to_bool v then Jump_abs arg else Continue
  | LOAD_GLOBAL -> load_global t ~arg
  | CONTINUE_LOOP -> failwith "Unsupported: CONTINUE_LOOP"
  | SETUP_LOOP -> failwith "Unsupported: SETUP_LOOP"
  | SETUP_EXCEPT -> failwith "Unsupported: SETUP_EXCEPT"
  | SETUP_FINALLY -> failwith "Unsupported: SETUP_FINALLY"
  | LOAD_FAST -> load_fast t ~arg
  | STORE_FAST -> store_fast t ~arg
  | DELETE_FAST -> delete_fast t ~arg
  | RAISE_VARARGS -> failwith "Unsupported: RAISE_VARARGS"
  | CALL_FUNCTION -> call_function t ~arg
  | MAKE_FUNCTION ->
    let name = Stack.pop_exn t.stack |> Bc_value.str_exn in
    let code, args, to_capture = Stack.pop_exn t.stack |> Bc_value.code_exn in
    let defaults = popn t.stack (List.length args.kwonlyargs) in
    let defaults = List.zip_exn (List.map args.kwonlyargs ~f:fst) defaults in
    let captured =
      List.filter_map to_capture ~f:(fun name ->
          find_name t name |> Option.map ~f:(fun v -> name, v))
    in
    let value =
      Bc_value.Function { name; code; args; defaults; captured; method_self = None }
    in
    push_and_continue t.stack value
  | BUILD_SLICE -> failwith "Unsupported: BUILD_SLICE"
  | LOAD_CLOSURE -> failwith "Unsupported: LOAD_CLOSURE"
  | LOAD_DEREF -> failwith "Unsupported: LOAD_DEREF"
  | STORE_DEREF -> failwith "Unsupported: STORE_DEREF"
  | DELETE_DEREF -> failwith "Unsupported: DELETE_DEREF"
  | CALL_FUNCTION_KW -> call_function_kw t ~arg
  | CALL_FUNCTION_EX -> failwith "Unsupported: CALL_FUNCTION_EX"
  | SETUP_WITH -> failwith "Unsupported: SETUP_WITH"
  | EXTENDED_ARG -> failwith "Unsupported: EXTENDED_ARG"
  | LIST_APPEND -> list_append t.stack ~arg
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

type action =
  | Continue
  | Call_fn of
      { code : Bc_value.code
      ; local_scope : Bc_scope.t
      }
  | Return of Bc_value.t

let eval_step t =
  if t.counter >= Array.length t.code.opcodes
  then Return Bc_value.none
  else (
    let { Bc_code.opcode; arg; lineno = _ } = t.code.opcodes.(t.counter) in
    match eval_one t opcode ~arg with
    | Continue ->
      t.counter <- t.counter + 1;
      Continue
    | Call_fn { code; local_scope } ->
      t.counter <- t.counter + 1;
      Call_fn { code; local_scope }
    | Jump_rel i ->
      t.counter <- t.counter + i;
      Continue
    | Jump_abs a ->
      t.counter <- a;
      Continue
    | Return v -> Return v)

let function_call_returned t v = Stack.push t.stack v
let stack t = t.stack
let parent_frame t = t.parent_frame

let current_filename_and_lineno t =
  let lineno =
    if t.counter >= Array.length t.code.opcodes
    then None
    else Some t.code.opcodes.(t.counter).lineno
  in
  t.code.filename, lineno
