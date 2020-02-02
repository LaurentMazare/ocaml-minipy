open Base
open Ast

module Type_ = struct
  type t =
    | None_t
    | Bool
    | Int
    | Float
    | Tuple
    | List
    | Str
    | Builtin_fn
    | Function
  [@@deriving sexp]
end

type value =
  | Val_none
  | Val_bool of bool
  | Val_int of int
  | Val_float of float
  | Val_tuple of value array
  | Val_list of value array
  | Val_str of string
  | Val_builtin_fn of (value list -> value)
  | Val_function of
      { args : string list
      ; body : stmt list
      }
[@@deriving sexp]

let type_of = function
  | Val_none -> Type_.None_t
  | Val_bool _ -> Bool
  | Val_int _ -> Int
  | Val_float _ -> Float
  | Val_tuple _ -> Tuple
  | Val_list _ -> List
  | Val_str _ -> Str
  | Val_builtin_fn _ -> Builtin_fn
  | Val_function _ -> Function

let type_as_string value = type_of value |> Type_.sexp_of_t |> Sexp.to_string_mach

let value_to_bool v =
  match v with
  | Val_bool b -> b
  | Val_int i -> i <> 0
  | Val_float f -> Float.( <> ) f 0.
  | Val_list l | Val_tuple l -> not (Array.is_empty l)
  | Val_str s -> not (String.is_empty s)
  | Val_function _ | Val_builtin_fn _ | Val_none ->
    Printf.failwithf "not a bool %s" (sexp_of_value v |> Sexp.to_string_mach) ()

let value_to_float v =
  match v with
  | Val_bool true -> 1.
  | Val_bool false -> 0.
  | Val_float f -> f
  | Val_int i -> Float.of_int i
  | v -> Printf.failwithf "not a bool %s" (sexp_of_value v |> Sexp.to_string_mach) ()

let apply_subscript ~value ~index =
  match value, index with
  | Val_tuple v, Val_int i | Val_list v, Val_int i ->
    let v_len = Array.length v in
    if 0 <= i && i < v_len
    then v.(i)
    else if i < 0 && -v_len <= i
    then v.(v_len + i)
    else Printf.failwithf "unexpected index %d for an array of length %d" i v_len ()
  | _ ->
    Printf.failwithf
      "not implemented: %s[%s]"
      (type_as_string value)
      (type_as_string index)
      ()

let apply_subscript_assign ~lvalue ~slice ~rvalue =
  match lvalue, slice with
  | Val_list v, Val_int i ->
    let v_len = Array.length v in
    if 0 <= i && i < v_len
    then v.(i) <- rvalue
    else if i < 0 && -v_len <= i
    then v.(v_len + i) <- rvalue
    else Printf.failwithf "unexpected index %d for an array of length %d" i v_len ()
  | _ ->
    Printf.failwithf
      "not implemented: %s[%s] assign"
      (type_as_string lvalue)
      (type_as_string slice)
      ()

let apply_unary_op op operand =
  match op, operand with
  | UAdd, (Val_int _ as v) -> v
  | UAdd, (Val_float _ as v) -> v
  | USub, Val_int v -> Val_int (-v)
  | USub, Val_float v -> Val_float (-.v)
  | _ ->
    Printf.failwithf
      "unary op not implemented: %s %s"
      (sexp_of_unaryop op |> Sexp.to_string_mach)
      (type_as_string operand)
      ()

let apply_op op left right =
  match op, left, right with
  | Add, Val_int v, Val_int v' -> Val_int (v + v')
  | Add, Val_float v, v' | Add, v', Val_float v -> Val_float (v +. value_to_float v')
  | Sub, Val_int v, Val_int v' -> Val_int (v - v')
  | Sub, Val_float v, v' -> Val_float (v -. value_to_float v')
  | Sub, v, Val_float v' -> Val_float (value_to_float v -. v')
  | Mult, Val_int v, Val_int v' -> Val_int (v * v')
  | Mult, Val_float v, v' | Mult, v', Val_float v -> Val_float (v *. value_to_float v')
  | Div, v, v' -> Val_float (value_to_float v /. value_to_float v')
  | Mod, Val_int v, Val_int v' -> Val_int (v % v')
  | _ ->
    Printf.failwithf
      "binop not implemented: %s %s %s"
      (type_as_string left)
      (sexp_of_operator op |> Sexp.to_string_mach)
      (type_as_string right)
      ()

let apply_comp op left right =
  match op with
  | Eq -> Caml.( = ) left right
  | NotEq -> Caml.( <> ) left right
  | Lt -> Caml.( < ) left right
  | LtE -> Caml.( <= ) left right
  | Gt -> Caml.( > ) left right
  | GtE -> Caml.( >= ) left right
  | _ ->
    Printf.failwithf
      "comparison not implemented: %s %s %s"
      (type_as_string left)
      (sexp_of_cmpop op |> Sexp.to_string_mach)
      (type_as_string right)
      ()

exception Return_exn of value
exception Break
exception Continue

type builtins = (string, value list -> value, String.comparator_witness) Map.t

module Env : sig
  type t

  val empty : builtins:builtins -> t

  (* [body] is used to extract local variables. *)
  val nest : prev_env:t -> body:stmt list -> t
  val find_exn : t -> name:string -> value
  val set : t -> name:string -> value:value -> unit
end = struct
  type t =
    { scope : (string, value) Hashtbl.t
    ; prev_env : t option
    ; local_variables : string Hash_set.t
    ; builtins : builtins
    }

  let empty ~builtins =
    { scope = Hashtbl.create (module String)
    ; prev_env = None
    ; local_variables = Hash_set.create (module String)
    ; builtins
    }

  let local_variables body =
    let local_variables = Hash_set.create (module String) in
    let rec loop = function
      | If { test = _; body; orelse } | While { test = _; body; orelse } ->
        List.iter body ~f:loop;
        List.iter orelse ~f:loop
      | For { target; iter = _; body; orelse } ->
        loop_expr target;
        List.iter body ~f:loop;
        List.iter orelse ~f:loop
      | Assign { targets; value = _ } -> List.iter targets ~f:loop_expr
      | Return _ | Delete _ | Expr _ | FunctionDef _ | Break | Continue | Pass -> ()
    and loop_expr = function
      | Name name -> Hash_set.add local_variables name
      | List l | Tuple l -> Array.iter l ~f:loop_expr
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

  let nest ~prev_env ~body =
    { scope = Hashtbl.create (module String)
    ; prev_env = Some prev_env
    ; local_variables = local_variables body
    ; builtins = Map.empty (module String)
    }

  let set t ~name ~value = Hashtbl.set t.scope ~key:name ~data:value

  let find_exn t ~name =
    if Hash_set.mem t.local_variables name && not (Hashtbl.mem t.scope name)
    then Printf.failwithf "Variable %s accessed before being initialized" name ();
    let rec loop t =
      match Hashtbl.find t.scope name with
      | Some value -> value
      | None ->
        (match t.prev_env with
        | Some t -> loop t
        | None ->
          (match Map.find t.builtins name with
          | Some value -> Val_builtin_fn value
          | None -> Printf.failwithf "cannot find variable %s in scopes" name ()))
    in
    loop t
end

(* Very naive evaluation. *)
let rec eval_stmt env = function
  | Expr { value } -> ignore (eval_expr env value : value)
  | FunctionDef { name; args; body } ->
    Env.set env ~name ~value:(Val_function { args; body })
  | While { test; body; orelse } ->
    let rec loop () =
      if eval_expr env test |> value_to_bool
      then (
        try
          eval_stmts env body;
          loop ()
        with
        | Break -> ()
        | Continue -> loop ())
      else eval_stmts env orelse
    in
    loop ()
  | For { target; iter; body; orelse } ->
    let iter =
      match eval_expr env iter with
      | Val_list l | Val_tuple l -> l
      | o -> Printf.failwithf "not implemented: for on %s" (type_as_string o) ()
    in
    let iter_len = Array.length iter in
    let rec loop index =
      if index < iter_len
      then (
        eval_assign env ~target ~value:iter.(index);
        try
          eval_stmts env body;
          loop (index + 1)
        with
        | Break -> ()
        | Continue -> loop (index + 1))
      else eval_stmts env orelse
    in
    loop 0
  | If { test; body; orelse } ->
    if eval_expr env test |> value_to_bool
    then eval_stmts env body
    else eval_stmts env orelse
  | Assign { targets; value } ->
    let value = eval_expr env value in
    List.iter targets ~f:(fun target -> eval_assign env ~target ~value)
  | Return { value } ->
    raise (Return_exn (Option.value_map value ~f:(eval_expr env) ~default:Val_none))
  | Delete _ -> failwith "TODO Delete"
  | Break -> raise Break
  | Continue -> raise Continue
  | Pass -> ()

and eval_expr env = function
  | Bool b -> Val_bool b
  | Num n -> Val_int n
  | Float f -> Val_float f
  | Str s -> Val_str s
  | List l -> Val_list (Array.map l ~f:(eval_expr env))
  | Tuple l -> Val_tuple (Array.map l ~f:(eval_expr env))
  | Name name -> Env.find_exn env ~name
  | BoolOp { op = And; values } ->
    Val_bool (List.for_all values ~f:(fun v -> eval_expr env v |> value_to_bool))
  | BoolOp { op = Or; values } ->
    Val_bool (List.exists values ~f:(fun v -> eval_expr env v |> value_to_bool))
  | UnaryOp { op; operand } ->
    let operand = eval_expr env operand in
    apply_unary_op op operand
  | BinOp { left; op; right } ->
    let left = eval_expr env left in
    let right = eval_expr env right in
    apply_op op left right
  | IfExp { test; body; orelse } ->
    if eval_expr env test |> value_to_bool
    then eval_expr env body
    else eval_expr env orelse
  | Compare { left; ops; comparators } ->
    let left = eval_expr env left in
    let right = eval_expr env comparators in
    Val_bool (apply_comp ops left right)
  | Call { func; args } ->
    let func = eval_expr env func in
    let arg_values = List.map args ~f:(eval_expr env) in
    (match func with
    | Val_builtin_fn fn -> fn arg_values
    | Val_function { args; body } ->
      let env = Env.nest ~prev_env:env ~body in
      let res =
        List.iter2 args arg_values ~f:(fun name value -> Env.set env ~name ~value)
      in
      (match res with
      | Ok () ->
        (try
           eval_stmts env body;
           Val_none
         with
        | Return_exn value -> value)
      | Unequal_lengths ->
        Printf.failwithf
          "expected %d arguments, got %d"
          (List.length args)
          (List.length arg_values)
          ())
    | _ -> failwith "not a function")
  | Attribute { value = _; attr = _ } -> failwith "TODO attribute"
  | Subscript { value; slice } ->
    let value = eval_expr env value in
    let index = eval_expr env slice in
    apply_subscript ~value ~index

and eval_stmts env stmts = List.iter stmts ~f:(eval_stmt env)

and eval_assign env ~target ~value =
  match target with
  | Name name -> Env.set env ~name ~value
  | Subscript { value = lvalue; slice } ->
    let lvalue = eval_expr env lvalue in
    let slice = eval_expr env slice in
    apply_subscript_assign ~lvalue ~slice ~rvalue:value
  | Tuple lvalues | List lvalues ->
    (match value with
    | Val_tuple rvalues | Val_list rvalues ->
      if Array.length rvalues <> Array.length lvalues
      then
        Printf.failwithf
          "different sizes on both sides of the assignment %d <> %d"
          (Array.length lvalues)
          (Array.length rvalues)
          ();
      Array.iter2_exn lvalues rvalues ~f:(fun target value ->
          eval_assign env ~target ~value)
    | _ -> failwith "cannot unpack for assignment")
  | _ -> failwith "TODO Generic Assign"

let default_builtins : builtins =
  let print args =
    [%sexp_of: value list] args |> Sexp.to_string_mach |> Stdio.printf "%s\n";
    Val_none
  in
  Map.of_alist_exn (module String) [ "print", print ]

let simple_eval ?(builtins = default_builtins) t =
  let env = Env.empty ~builtins in
  eval_stmts env t

let simple_eval_expr ?(builtins = default_builtins) t =
  let env = Env.empty ~builtins in
  eval_expr env t
