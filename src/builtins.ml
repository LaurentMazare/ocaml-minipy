open! Base
open! Import

let to_int v = Value.to_int v |> Z.to_int
let of_int i = Z.of_int i |> Value.int

let empty_kwargs kwargs ~name =
  if not (Hashtbl.is_empty kwargs) then errorf "%s expects no keyword arguments" name

let print args kwargs =
  empty_kwargs kwargs ~name:"print";
  List.map args ~f:(Value.to_string ~escape_special_chars:false)
  |> String.concat ~sep:" "
  |> Stdio.printf "%s\n";
  Value.none

let range args kwargs =
  empty_kwargs kwargs ~name:"range";
  let l =
    match args with
    | [ v ] -> List.range 0 (to_int v)
    | [ v1; v2 ] -> List.range (to_int v1) (to_int v2)
    | [ v1; v2; s ] -> List.range (to_int v1) (to_int v2) ~stride:(to_int s)
    | _ -> errorf "range expects one, two, or three arguments"
  in
  Value.list (List.map l ~f:of_int |> Queue.of_list)

let len args kwargs =
  empty_kwargs kwargs ~name:"len";
  let l =
    match (args : Value.t list) with
    | [ Val_tuple l ] -> Array.length l
    | [ Val_list l ] -> Queue.length l
    | [ Val_str s ] -> String.length s
    | [ Val_dict d ] -> Hashtbl.length d
    | [ v ] -> Value.cannot_be_interpreted_as v "type with len"
    | _ -> errorf "len takes exactly one argument"
  in
  of_int l

let delattr args kwargs =
  empty_kwargs kwargs ~name:"delattr";
  match (args : Value.t list) with
  | [ Val_object { attrs; _ }; Val_str key ] ->
    Hashtbl.remove attrs key;
    Value.none
  | _ -> errorf "delattr takes exactly two arguments (object, str)"

let getattr args kwargs =
  empty_kwargs kwargs ~name:"getattr";
  match (args : Value.t list) with
  | [ Val_object { attrs; _ }; Val_str key ] ->
    (match Hashtbl.find attrs key with
    | Some value -> value
    | None -> errorf "object has no attribute %s" key)
  | [ Val_object { attrs; _ }; Val_str key; default ] ->
    Hashtbl.find attrs key |> Option.value ~default
  | _ ->
    errorf
      "getattr takes two arguments (object, str) or three arguments (object, str, \
       default)"

let hasattr args kwargs =
  empty_kwargs kwargs ~name:"hasattr";
  match (args : Value.t list) with
  | [ Val_object { attrs; _ }; Val_str key ] -> Value.bool (Hashtbl.mem attrs key)
  | _ -> errorf "hasattr takes exactly two arguments (object, str)"

let setattr args kwargs =
  empty_kwargs kwargs ~name:"setattr";
  match (args : Value.t list) with
  | [ Val_object { attrs; _ }; Val_str key; data ] ->
    Hashtbl.set attrs ~key ~data;
    Value.none
  | _ -> errorf "setattr takes exactly three arguments (object, str, value)"

let isinstance args kwargs =
  empty_kwargs kwargs ~name:"isinstance";
  match (args : Value.t list) with
  | [ Val_object { cls; _ }; v ] ->
    let rec loop : Value.t -> bool = function
      | Val_tuple vs -> Array.exists vs ~f:loop
      | Val_class target_class -> Value.is_subclass cls ~target_class
      | _ -> errorf "isinstance only accepts tuples or classes for its second argument"
    in
    Value.bool (loop v)
  | _ -> errorf "isinstance takes exactly two arguments (object, class)"

let issubclass args kwargs =
  empty_kwargs kwargs ~name:"issubclass";
  match (args : Value.t list) with
  | [ Val_class cls; v ] ->
    let rec loop : Value.t -> bool = function
      | Val_tuple vs -> Array.exists vs ~f:loop
      | Val_class target_class -> Value.is_subclass cls ~target_class
      | _ -> errorf "issubclass only accepts tuples or classes for its second argument"
    in
    Value.bool (loop v)
  | _ -> errorf "issubclass takes exactly two arguments (class1, class2)"

let str args kwargs =
  empty_kwargs kwargs ~name:"str";
  match (args : Value.t list) with
  | [ v ] -> Value.to_string ~escape_special_chars:true v |> Value.str
  | _ -> errorf "str takes exactly one argument"

let int args kwargs =
  empty_kwargs kwargs ~name:"int";
  let v =
    match (args : Value.t list) with
    | [ Val_int v ] -> v
    | [ Val_float f ] -> Z.of_float f
    | [ Val_bool true ] -> Z.one
    | [ Val_bool false ] -> Z.zero
    | [ Val_str s ] -> Z.of_string s
    | _ -> errorf "int takes exactly one argument"
  in
  Value.int v

let float args kwargs =
  empty_kwargs kwargs ~name:"float";
  let v =
    match (args : Value.t list) with
    | [ Val_int v ] -> Z.to_float v
    | [ Val_float f ] -> f
    | [ Val_bool true ] -> 1.
    | [ Val_bool false ] -> 0.
    | [ Val_str s ] -> Float.of_string s
    | _ -> errorf "float takes exactly one argument"
  in
  Value.float v

let bool args kwargs =
  empty_kwargs kwargs ~name:"bool";
  match (args : Value.t list) with
  | [ v ] -> Value.bool (Value.to_bool v)
  | _ -> errorf "bool takes exactly one argument"

let default : Value.builtins =
  Map.of_alist_exn
    (module String)
    [ "print", print
    ; "range", range
    ; "len", len
    ; "delattr", delattr
    ; "getattr", getattr
    ; "setattr", setattr
    ; "hasattr", hasattr
    ; "isinstance", isinstance
    ; "issubclass", issubclass
    ; "str", str
    ; "int", int
    ; "bool", bool
    ; "float", float
    ]
