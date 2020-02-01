open Base
open Ast

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

let apply_op op left right =
  match op, left, right with
  | Add, Val_int v, Val_int v' -> Val_int (v + v')
  | Add, Val_float v, v' | Add, v', Val_float v -> Val_float (v +. value_to_float v')
  | Sub, Val_int v, Val_int v' -> Val_int (v + v')
  | Sub, Val_float v, v' -> Val_float (v -. value_to_float v')
  | Sub, v, Val_float v' -> Val_float (value_to_float v -. v')
  | Mult, Val_int v, Val_int v' -> Val_int (v * v')
  | Mult, Val_float v, v' | Mult, v', Val_float v -> Val_float (v *. value_to_float v')
  | Div, v, v' -> Val_float (value_to_float v /. value_to_float v')
  | _ -> failwith "TODO op"

let apply_comp op left right =
  match op with
  | Eq -> Caml.( = ) left right
  | NotEq -> Caml.( <> ) left right
  | _ -> failwith "TODO cmp"

exception Return_exn of value

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
      | Return _ | Delete _ | Expr _ | FunctionDef _ -> ()
      | If { test = _; body; orelse } | While { test = _; body; orelse } ->
        List.iter body ~f:loop;
        List.iter orelse ~f:loop
      | Assign { targets = [ Name name ]; value = _ } -> Hash_set.add local_variables name
      | Assign _ -> failwith "TODO Generic Assign"
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
        eval_stmts env body;
        loop ())
      else eval_stmts env orelse
    in
    loop ()
  | If { test; body; orelse } ->
    if eval_expr env test |> value_to_bool
    then eval_stmts env body
    else eval_stmts env orelse
  | Assign { targets = [ Name name ]; value } ->
    let value = eval_expr env value in
    Env.set env ~name ~value
  | Assign _ -> failwith "TODO Generic Assign"
  | Return { value } ->
    raise (Return_exn (Option.value_map value ~f:(eval_expr env) ~default:Val_none))
  | Delete _ -> failwith "TODO Delete"

and eval_expr env = function
  | Bool b -> Val_bool b
  | Num n -> Val_int n
  | Float f -> Val_float f
  | Str s -> Val_str s
  | Name name -> Env.find_exn env ~name
  | BoolOp { op = And; values } ->
    Val_bool (List.for_all values ~f:(fun v -> eval_expr env v |> value_to_bool))
  | BoolOp { op = Or; values } ->
    Val_bool (List.exists values ~f:(fun v -> eval_expr env v |> value_to_bool))
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

and eval_stmts env stmts = List.iter stmts ~f:(eval_stmt env)

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
