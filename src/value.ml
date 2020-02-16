open Base
open Ast
open Import

module Class_id : sig
  type t

  val create : unit -> t
  val equal : t -> t -> bool
end = struct
  type t = int

  let create =
    let counter = ref 0 in
    fun () ->
      Int.incr counter;
      !counter

  let equal = Int.equal
end

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

and builtin_fn = interp -> t list -> (string, t) Hashtbl.t -> t

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
  ; parent_class : cls option
  ; id : Class_id.t
  }

and interp =
  { call_method : t -> string -> t list -> t
  ; has_method : t -> string -> bool
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
    errorf "not implemented: %s[%s] assign" (type_as_string lvalue) (type_as_string slice)

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
  | Pow, Val_int v, Val_int v' -> Val_int (Z.pow v (Z.to_int v'))
  | Pow, v, v' -> Val_float (Float.( ** ) (to_float v) (to_float v'))
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

let rec is_subclass cls ~target_class =
  if Class_id.equal cls.id target_class.id
  then true
  else (
    match cls.parent_class with
    | None -> false
    | Some parent_cls -> is_subclass parent_cls ~target_class)

let is_instance t ~target_class =
  match t with
  | Val_object { cls; _ } -> is_subclass cls ~target_class
  | _ -> false

let is_instance_or_subclass t ~target_class =
  match t with
  | Val_class cls | Val_object { cls; _ } -> is_subclass cls ~target_class
  | _ -> false

let none = Val_none
let list l = Val_list l
let str s = Val_str s
let bool b = Val_bool b
let int i = Val_int i
let float f = Val_float f
let tuple t = Val_tuple t
let dict d = Val_dict d
let fn f = Val_function f
