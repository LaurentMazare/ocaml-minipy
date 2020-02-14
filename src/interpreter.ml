open Base
open Ast

exception RuntimeError of string

let errorf fmt = Printf.ksprintf (fun s -> raise (RuntimeError s)) fmt

module Type_ = struct
  type t =
    | None_t
    | Bool
    | Int
    | Float
    | Tuple
    | Dict
    | List
    | Str
    | Builtin_fn
    | Function
    | Object
    | Class
  [@@deriving sexp]

  let to_string t = sexp_of_t t |> Sexp.to_string_mach
end

module Value = struct
  type 'a dict = ('a, 'a) Hashtbl.Poly.t [@@deriving sexp]

  type t =
    | Val_none
    | Val_bool of bool
    | Val_int of Z.t
    | Val_float of float
    | Val_tuple of t array
    | Val_list of t Queue.t
    | Val_dict of t dict
    | Val_str of string
    | Val_class of cls
    | Val_object of
        { cls : cls
        ; attrs : (string, t) Hashtbl.t
        }
    | Val_builtin_fn of builtin_fn
    | Val_function of fn

  and builtin_fn = t list -> (string, t) Hashtbl.t -> t

  and builtins = (string, builtin_fn, String.comparator_witness) Map.t

  and env =
    { scope : (string, t) Hashtbl.t
    ; prev_env : env option
    ; local_variables : string Hash_set.t
    ; builtins : builtins
    }

  and fn =
    { args : arguments
    ; env : env
    ; body : stmt list
    ; method_self : t option
    }

  and cls =
    { name : string
    ; attrs : (string, t) Hashtbl.t
    }

  let to_string ?(escape_special_chars = true) t =
    let rec loop ~e = function
      | Val_none -> "None"
      | Val_bool true -> "True"
      | Val_bool false -> "False"
      | Val_int i -> Z.to_string i
      | Val_float f -> Float.to_string f
      | Val_tuple [| t |] -> "(" ^ loop ~e:true t ^ ",)"
      | Val_tuple ts ->
        Array.to_list ts
        |> List.map ~f:(loop ~e:true)
        |> String.concat ~sep:", "
        |> fun s -> "(" ^ s ^ ")"
      | Val_list ts ->
        Queue.to_list ts
        |> List.map ~f:(loop ~e:true)
        |> String.concat ~sep:", "
        |> fun s -> "[" ^ s ^ "]"
      | Val_dict dict ->
        Hashtbl.to_alist dict
        |> List.map ~f:(fun (key, value) -> loop ~e:true key ^ ": " ^ loop ~e:true value)
        |> String.concat ~sep:","
        |> fun s -> "{" ^ s ^ "}"
      | Val_str s -> if e then "\'" ^ String.escaped s ^ "\'" else s
      | Val_builtin_fn _ -> "<builtin>"
      | Val_function _ -> "<function>"
      | Val_class { name; _ } -> Printf.sprintf "<class %s>" name
      | Val_object { cls = { name; _ }; _ } -> Printf.sprintf "<object %s>" name
    in
    loop t ~e:escape_special_chars

  let type_ = function
    | Val_none -> Type_.None_t
    | Val_bool _ -> Bool
    | Val_int _ -> Int
    | Val_float _ -> Float
    | Val_tuple _ -> Tuple
    | Val_list _ -> List
    | Val_dict _ -> Dict
    | Val_str _ -> Str
    | Val_builtin_fn _ -> Builtin_fn
    | Val_function _ -> Function
    | Val_object _ -> Object
    | Val_class _ -> Class

  let type_as_string t = type_ t |> Type_.to_string

  let cannot_be_interpreted_as v str =
    errorf "%s cannot be interpreted as %s" (type_as_string v) str

  let to_bool v =
    match v with
    | Val_bool b -> b
    | Val_int i -> not Z.(equal i zero)
    | Val_float f -> Float.( <> ) f 0.
    | Val_tuple l -> not (Array.is_empty l)
    | Val_list l -> not (Queue.is_empty l)
    | Val_str s -> not (String.is_empty s)
    | v -> cannot_be_interpreted_as v "bool"

  let to_float v =
    match v with
    | Val_bool true -> 1.
    | Val_bool false -> 0.
    | Val_float f -> f
    | Val_int i -> Z.to_float i
    | v -> cannot_be_interpreted_as v "float"

  let to_int v =
    match v with
    | Val_bool true -> Z.one
    | Val_bool false -> Z.zero
    | Val_int i -> i
    | v -> cannot_be_interpreted_as v "int"

  let to_iterable v =
    match v with
    | Val_list l -> Queue.to_array l
    | Val_tuple l -> l
    | Val_str s -> String.to_array s |> Array.map ~f:(fun c -> Val_str (Char.to_string c))
    | Val_dict s -> Hashtbl.keys s |> Array.of_list
    | o -> cannot_be_interpreted_as o "iterable"

  let apply_subscript ~value ~index =
    match value, index with
    | Val_tuple v, Val_int i ->
      let i = Z.to_int i in
      let v_len = Array.length v in
      if 0 <= i && i < v_len
      then v.(i)
      else if i < 0 && -v_len <= i
      then v.(v_len + i)
      else errorf "unexpected index %d for an array of length %d" i v_len
    | Val_list v, Val_int i ->
      let i = Z.to_int i in
      let v_len = Queue.length v in
      if 0 <= i && i < v_len
      then Queue.get v i
      else if i < 0 && -v_len <= i
      then Queue.get v (v_len + i)
      else errorf "unexpected index %d for an array of length %d" i v_len
    | Val_dict dict, i ->
      (match Hashtbl.find dict i with
      | Some v -> v
      | None -> errorf "KeyError: %s" (to_string i))
    | _ -> errorf "not implemented: %s[%s]" (type_as_string value) (type_as_string index)

  let apply_subscript_assign ~lvalue ~slice ~rvalue =
    match lvalue, slice with
    | Val_list v, Val_int i ->
      let i = Z.to_int i in
      let v_len = Queue.length v in
      if 0 <= i && i < v_len
      then Queue.set v i rvalue
      else if i < 0 && -v_len <= i
      then Queue.set v (v_len + i) rvalue
      else errorf "unexpected index %d for an array of length %d" i v_len
    | Val_dict dict, key -> Hashtbl.set dict ~key ~data:rvalue
    | _ ->
      errorf
        "not implemented: %s[%s] assign"
        (type_as_string lvalue)
        (type_as_string slice)

  let apply_unary_op op operand =
    match op, operand with
    | UAdd, (Val_int _ as v) -> v
    | UAdd, (Val_float _ as v) -> v
    | USub, Val_int v -> Val_int (Z.neg v)
    | USub, Val_float v -> Val_float (-.v)
    | Not, v -> Val_bool (not (to_bool v))
    | _ ->
      errorf
        "unary op not implemented: %s %s"
        (sexp_of_unaryop op |> Sexp.to_string_mach)
        (type_as_string operand)

  let apply_op op left right =
    match op, left, right with
    | Add, Val_int v, Val_int v' -> Val_int (Z.add v v')
    | Add, Val_float v, v' | Add, v', Val_float v -> Val_float (v +. to_float v')
    | Add, Val_str s, Val_str s' -> Val_str (s ^ s')
    | Add, Val_list a, Val_list a' ->
      Val_list (Array.append (Queue.to_array a) (Queue.to_array a') |> Queue.of_array)
    | Sub, Val_int v, Val_int v' -> Val_int (Z.sub v v')
    | Sub, Val_float v, v' -> Val_float (v -. to_float v')
    | Sub, v, Val_float v' -> Val_float (to_float v -. v')
    | Mult, Val_int v, Val_int v' -> Val_int (Z.mul v v')
    | Mult, Val_float v, v' | Mult, v', Val_float v -> Val_float (v *. to_float v')
    | Mult, Val_list a, Val_int n ->
      let n = Z.to_int n in
      let a = Queue.to_array a in
      List.init n ~f:(fun _ -> a) |> Array.concat |> fun a -> Val_list (Queue.of_array a)
    | Div, v, v' -> Val_float (to_float v /. to_float v')
    | FloorDiv, Val_int v, Val_int v' -> Val_int (Z.div v v')
    | Mod, Val_int v, Val_int v' -> Val_int (Z.( mod ) v v')
    | _ ->
      errorf
        "binop not implemented: %s %s %s"
        (type_as_string left)
        (sexp_of_operator op |> Sexp.to_string_mach)
        (type_as_string right)

  let apply_comp op left right =
    match op with
    | Eq -> Caml.( = ) left right
    | NotEq -> Caml.( <> ) left right
    | Lt -> Caml.( < ) left right
    | LtE -> Caml.( <= ) left right
    | Gt -> Caml.( > ) left right
    | GtE -> Caml.( >= ) left right
    | In -> Array.mem (to_iterable right) left ~equal:Caml.( = )
    | NotIn -> not (Array.mem (to_iterable right) left ~equal:Caml.( = ))
    | _ ->
      errorf
        "comparison not implemented: %s %s %s"
        (type_as_string left)
        (sexp_of_cmpop op |> Sexp.to_string_mach)
        (type_as_string right)

  let none = Val_none
  let list l = Val_list l
  let str s = Val_str s
  let bool b = Val_bool b
  let int i = Val_int i
  let float f = Val_float f
  let tuple t = Val_tuple t
  let dict d = Val_dict d
  let fn f = Val_function f
end

exception Return_exn of Value.t
exception Break
exception Continue
exception Assert of Value.t

exception
  Raise of
    { exc : Value.t option
    ; cause : Value.t option
    }

type builtins = Value.builtins

module Env : sig
  type t = Value.env

  val empty : builtins:builtins -> t

  (* [body] is used to extract local variables. *)
  val nest : prev_env:t -> body:stmt list -> t
  val find_exn : t -> name:string -> Value.t
  val set : t -> name:string -> value:Value.t -> unit
  val remove : t -> name:string -> unit
  val last_scope : t -> (string, Value.t) Hashtbl.t
end = struct
  type t = Value.env =
    { scope : (string, Value.t) Hashtbl.t
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

  let last_scope t = t.scope

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
      | Try { body; orelse; finalbody; handlers } ->
        List.iter body ~f:loop;
        List.iter orelse ~f:loop;
        List.iter finalbody ~f:loop;
        List.iter handlers ~f:(fun handler -> List.iter handler.body ~f:loop)
      | Assign { targets; value = _ } -> List.iter targets ~f:loop_expr
      | AugAssign { target = _; op = _; value = _ } -> ()
      (* Augmented assign does not create a new bindings but replaces an existing one. *)
      | ClassDef { name; _ } | FunctionDef { name; _ } ->
        Hash_set.add local_variables name
      | Assert _ | Return _ | Delete _ | Expr _ | Raise _ | Break | Continue | Pass -> ()
    and loop_expr = function
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

  let nest ~prev_env ~body =
    { scope = Hashtbl.create (module String)
    ; prev_env = Some prev_env
    ; local_variables = local_variables body
    ; builtins = Map.empty (module String)
    }

  let set t ~name ~value = Hashtbl.set t.scope ~key:name ~data:value

  let remove t ~name =
    match Hashtbl.find_and_remove t.scope name with
    | None -> errorf "Variable %s is not defined" name
    | Some _ -> ()

  let find_exn t ~name =
    if Hash_set.mem t.local_variables name && not (Hashtbl.mem t.scope name)
    then errorf "Variable %s accessed before being initialized" name ();
    let rec loop t =
      match Hashtbl.find t.scope name with
      | Some value -> value
      | None ->
        (match t.prev_env with
        | Some t -> loop t
        | None ->
          (match Map.find t.builtins name with
          | Some value -> Val_builtin_fn value
          | None -> errorf "cannot find variable %s in scopes" name ()))
    in
    loop t
end

let list_attrs queue ~attr =
  match attr with
  | "append" ->
    let append args kwargs =
      if not (Hashtbl.is_empty kwargs) then errorf "reverse expects no keyword argument";
      match args with
      | [ a ] ->
        Queue.enqueue queue a;
        Value.none
      | _ -> errorf "append expects a single argument, got %d" (List.length args)
    in
    Value.Val_builtin_fn append
  | "clear" ->
    let clear args kwargs =
      if not (List.is_empty args) then errorf "clear expects no argument";
      if not (Hashtbl.is_empty kwargs) then errorf "reverse expects no keyword argument";
      Queue.clear queue;
      Value.none
    in
    Value.Val_builtin_fn clear
  | "pop" ->
    let pop args kwargs =
      if not (List.is_empty args) then errorf "pop expects no argument";
      if not (Hashtbl.is_empty kwargs) then errorf "reverse expects no keyword argument";
      match Queue.dequeue queue with
      | None -> errorf "pop from empty list"
      | Some v -> v
    in
    Value.Val_builtin_fn pop
  | "reverse" ->
    let reverse args kwargs =
      if not (List.is_empty args) then errorf "reverse expects no argument";
      if not (Hashtbl.is_empty kwargs) then errorf "reverse expects no keyword argument";
      let result = Queue.create () in
      let nelems = Queue.length queue in
      for i = 0 to nelems - 1 do
        Queue.enqueue result (Queue.get queue (nelems - i - 1))
      done;
      Queue.clear queue;
      Queue.blit_transfer ~src:result ~dst:queue ();
      Value.none
    in
    Value.Val_builtin_fn reverse
  | "sort" ->
    let sort args kwargs =
      if not (List.is_empty args) then errorf "reverse expects no argument";
      if not (Hashtbl.is_empty kwargs) then errorf "reverse expects no keyword argument";
      let sorted = Queue.to_array queue in
      Array.sort sorted ~compare:Poly.compare;
      Queue.clear queue;
      for i = 0 to Array.length sorted - 1 do
        Queue.enqueue queue sorted.(i)
      done;
      Value.none
    in
    Value.Val_builtin_fn sort
  | attr -> errorf "'sort' object has no attribute '%s'" attr

(* Very naive evaluation. *)
let rec eval_stmt env = function
  | Expr { value } -> ignore (eval_expr env value : Value.t)
  | FunctionDef { name; args; body } ->
    Env.set env ~name ~value:(Val_function { args; env; body; method_self = None })
  | ClassDef { name; args = _; body } ->
    (* TODO: inheritance *)
    (* TODO: capture variables from above scopes *)
    let cls_env = Env.nest ~prev_env:env ~body in
    eval_stmts cls_env body;
    let attrs = Env.last_scope cls_env in
    Env.set env ~name ~value:(Val_class { name; attrs })
  | Try { body; handlers; orelse; finalbody } ->
    let raised =
      try
        eval_stmts env body;
        false
      with
      | _exn ->
        List.fold_until
          handlers
          ~init:()
          ~f:(fun () handler ->
            let { Ast.type_; name = _; body } = handler in
            match type_ with
            | None ->
              eval_stmts env body;
              Stop ()
            | Some _expr ->
              (* TODO: exception handlers *)
              Continue ())
          ~finish:Fn.id;
        true
    in
    if not raised then eval_stmts env orelse;
    eval_stmts env finalbody
  | Raise { exc; cause } ->
    let exc = Option.map exc ~f:(eval_expr env) in
    let cause = Option.map cause ~f:(eval_expr env) in
    raise (Raise { exc; cause })
  | While { test; body; orelse } ->
    let rec loop () =
      if eval_expr env test |> Value.to_bool
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
    let iter = eval_expr env iter |> Value.to_iterable in
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
    if eval_expr env test |> Value.to_bool
    then eval_stmts env body
    else eval_stmts env orelse
  | Assign { targets; value } ->
    let value = eval_expr env value in
    List.iter targets ~f:(fun target -> eval_assign env ~target ~value)
  | AugAssign { target; op; value } ->
    let value = eval_expr env value in
    let value = Value.apply_op op (eval_expr env target) value in
    eval_assign env ~target ~value
  | Return { value } ->
    raise (Return_exn (Option.value_map value ~f:(eval_expr env) ~default:Value.none))
  | Delete { targets } -> List.iter targets ~f:(delete env)
  | Break -> raise Break
  | Continue -> raise Continue
  | Pass -> ()
  | Assert { test; msg } ->
    if not (eval_expr env test |> Value.to_bool)
    then (
      let msg = Option.value_map msg ~f:(eval_expr env) ~default:Value.none in
      raise (Assert msg))

and eval_expr env = function
  | None_ -> Value.none
  | Bool b -> Value.bool b
  | Num n -> Value.int n
  | Float f -> Value.float f
  | Str s -> Value.str s
  | List l -> Value.list (Array.map l ~f:(eval_expr env) |> Queue.of_array)
  | Dict { key_values } ->
    let dict =
      List.map key_values ~f:(fun (key, value) -> eval_expr env key, eval_expr env value)
      |> Hashtbl.Poly.of_alist
      (* In python 3.7 a duplicate key is not an error *)
    in
    (match dict with
    | `Ok dict -> Value.dict dict
    | `Duplicate_key key -> errorf "duplicate key %s" (Value.to_string key) ())
  | ListComp { elt; generators } -> eval_list_comp env ~elt ~generators
  | Tuple l -> Value.tuple (Array.map l ~f:(eval_expr env))
  | Name name -> Env.find_exn env ~name
  | BoolOp { op = And; values } ->
    Value.bool (List.for_all values ~f:(fun v -> eval_expr env v |> Value.to_bool))
  | BoolOp { op = Or; values } ->
    Value.bool (List.exists values ~f:(fun v -> eval_expr env v |> Value.to_bool))
  | UnaryOp { op; operand } ->
    let operand = eval_expr env operand in
    Value.apply_unary_op op operand
  | BinOp { left; op; right } ->
    let left = eval_expr env left in
    let right = eval_expr env right in
    Value.apply_op op left right
  | IfExp { test; body; orelse } ->
    if eval_expr env test |> Value.to_bool
    then eval_expr env body
    else eval_expr env orelse
  | Compare { left; ops; comparators } ->
    let left = eval_expr env left in
    let right = eval_expr env comparators in
    Value.bool (Value.apply_comp ops left right)
  | Call { func; args; keywords } ->
    let func = eval_expr env func in
    let arg_values = List.map args ~f:(eval_expr env) in
    let keyword_values =
      List.map keywords ~f:(fun (name, expr) -> name, eval_expr env expr)
      |> Hashtbl.of_alist_exn (module String)
    in
    (match func with
    | Val_builtin_fn fn -> fn arg_values keyword_values
    | Val_function { args; env; body; method_self } ->
      let arg_values =
        match method_self with
        | None -> arg_values
        | Some self -> self :: arg_values
      in
      let env = call_env ~prev_env:env ~body ~args ~arg_values ~keyword_values in
      (try
         eval_stmts env body;
         Value.none
       with
      | Return_exn value -> value)
    | Val_class ({ name = _; attrs } as cls) ->
      let self_attrs = Hashtbl.create (module String) in
      let self = Value.Val_object { cls; attrs = self_attrs } in
      Hashtbl.iteri attrs ~f:(fun ~key ~data ->
          let data =
            match data with
            | Val_function fn -> Value.Val_function { fn with method_self = Some self }
            | data -> data
          in
          Hashtbl.set self_attrs ~key ~data);
      (match Hashtbl.find attrs "__init__" with
      | None -> ()
      | Some (Val_function { args; env; body; method_self = _ }) ->
        let arg_values = self :: arg_values in
        let env = call_env ~prev_env:env ~body ~args ~arg_values ~keyword_values in
        (try eval_stmts env body with
        | Return_exn _ -> ())
      | Some v -> Value.cannot_be_interpreted_as v "callable");
      self
    | v -> Value.cannot_be_interpreted_as v "callable")
  | Attribute { value; attr } ->
    let value = eval_expr env value in
    (match value with
    | Val_object { attrs; _ } | Val_class { attrs; _ } ->
      (match Hashtbl.find attrs attr with
      | Some v -> v
      | None ->
        errorf "'%s' object has no attribute '%s'" (Value.type_as_string value) attr)
    | Val_list q -> list_attrs q ~attr
    | v -> errorf "'%s' object has no attribute '%s'" (Value.type_as_string v) attr)
  | Subscript { value; slice } ->
    let value = eval_expr env value in
    let index = eval_expr env slice in
    Value.apply_subscript ~value ~index
  | Lambda { args; body } ->
    Value.fn { args; env; body = [ Return { value = Some body } ]; method_self = None }

and eval_stmts env stmts = List.iter stmts ~f:(eval_stmt env)

and eval_assign env ~target ~value =
  match target with
  | Name name -> Env.set env ~name ~value
  | Subscript { value = lvalue; slice } ->
    let lvalue = eval_expr env lvalue in
    let slice = eval_expr env slice in
    Value.apply_subscript_assign ~lvalue ~slice ~rvalue:value
  | Tuple lvalues | List lvalues ->
    let rvalues =
      match value with
      | Val_tuple rvalues -> rvalues
      | Val_list rvalues -> Queue.to_array rvalues
      | v -> Value.cannot_be_interpreted_as v "cannot unpack for assignment"
    in
    if Array.length rvalues <> Array.length lvalues
    then
      errorf
        "different sizes on both sides of the assignment %d <> %d"
        (Array.length lvalues)
        (Array.length rvalues);
    Array.iter2_exn lvalues rvalues ~f:(fun target value ->
        eval_assign env ~target ~value)
  | Attribute { value = target; attr } ->
    (match eval_expr env target with
    | Val_object { attrs; _ } | Val_class { attrs; _ } ->
      Hashtbl.set attrs ~key:attr ~data:value
    | v -> errorf "'%s' object has no attribute '%s'" (Value.type_as_string v) attr)
  | _ -> failwith "TODO Generic Assign"

and eval_list_comp env ~elt ~generators =
  let rec loop env generators =
    match generators with
    | [] -> [| eval_expr env elt |]
    | { target; iter; ifs } :: generators ->
      let iter = eval_expr env iter |> Value.to_iterable in
      Array.concat_map iter ~f:(fun value ->
          let env = Env.nest ~prev_env:env ~body:[] in
          eval_assign env ~target ~value;
          let ifs = List.for_all ifs ~f:(fun if_ -> eval_expr env if_ |> Value.to_bool) in
          if ifs then loop env generators else [||])
  in
  Value.list (loop env generators |> Queue.of_array)

and delete env expr =
  match expr with
  | Name name -> Env.remove env ~name
  | Subscript { value; slice } ->
    (match eval_expr env value, eval_expr env slice with
    | Val_list _array, Val_int _i -> errorf "TODO: implement list deletion"
    | Val_dict dict, v -> Hashtbl.remove dict v
    | v, _ -> Value.cannot_be_interpreted_as v "cannot delete")
  | _ -> errorf "only names and subscripts can be deleted"

and call_env ~prev_env ~body ~args ~arg_values ~keyword_values =
  (* The semantic below is different from Python's implementation. *)
  let env = Env.nest ~prev_env ~body in
  let rec loop arg_and_expr =
    match arg_and_expr with
    | [], [] | _ :: _, [] | [], _ :: _ -> arg_and_expr
    | name :: args, value :: arg_values ->
      Env.set env ~name ~value;
      loop (args, arg_values)
  in
  let pos_args, pos_exprs = loop (args.args, arg_values) in
  List.iter pos_args ~f:(fun name ->
      match Hashtbl.find_and_remove keyword_values name with
      | None ->
        errorf
          "function takes %d positional arguments but %d were given"
          (List.length args.args)
          (List.length arg_values)
      | Some value -> Env.set env ~name ~value);
  (match args.vararg with
  | Some name -> Env.set env ~name ~value:(Val_list (Queue.of_list pos_exprs))
  | None ->
    if not (List.is_empty pos_exprs)
    then
      errorf
        "function takes %d positional arguments but %d were given"
        (List.length args.args)
        (List.length arg_values));
  List.iter args.kwonlyargs ~f:(fun (name, default_value) ->
      let value =
        match Hashtbl.find_and_remove keyword_values name with
        | None -> eval_expr env default_value
        | Some value -> value
      in
      Env.set env ~name ~value);
  (match args.kwarg with
  | Some name ->
    let dict =
      Hashtbl.to_alist keyword_values
      |> List.map ~f:(fun (name, value) -> Value.Val_str name, value)
      |> Hashtbl.Poly.of_alist_exn
    in
    Env.set env ~name ~value:(Val_dict dict)
  | None ->
    if not (Hashtbl.is_empty keyword_values)
    then
      errorf
        "function received too many keyword arguments %s"
        (Hashtbl.keys keyword_values |> String.concat ~sep:","));
  env

let default_builtins : builtins =
  let print args _kwargs =
    List.map args ~f:(Value.to_string ~escape_special_chars:false)
    |> String.concat ~sep:" "
    |> Stdio.printf "%s\n";
    Value.none
  in
  let to_int v = Value.to_int v |> Z.to_int in
  let of_int i = Z.of_int i |> Value.int in
  let range args _kwargs =
    let l =
      match args with
      | [ v ] -> List.range 0 (to_int v)
      | [ v1; v2 ] -> List.range (to_int v1) (to_int v2)
      | [ v1; v2; s ] -> List.range (to_int v1) (to_int v2) ~stride:(to_int s)
      | _ -> failwith "range expects one, two, or three arguments"
    in
    Value.list (List.map l ~f:of_int |> Queue.of_list)
  in
  let len args _kwargs =
    let l =
      match (args : Value.t list) with
      | [ Val_tuple l ] -> Array.length l
      | [ Val_list l ] -> Queue.length l
      | [ Val_str s ] -> String.length s
      | [ Val_dict d ] -> Hashtbl.length d
      | [ v ] -> Value.cannot_be_interpreted_as v "type with len"
      | _ -> failwith "len takes exactly one argument"
    in
    of_int l
  in
  Map.of_alist_exn (module String) [ "print", print; "range", range; "len", len ]

let simple_eval ?(builtins = default_builtins) t =
  let env = Env.empty ~builtins in
  eval_stmts env t

let simple_eval_expr ?(builtins = default_builtins) t =
  let env = Env.empty ~builtins in
  eval_expr env t
