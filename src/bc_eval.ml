open Base
open Import

let to_int v = Bc_value.to_int v |> Z.to_int

let range_fn _eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"range";
  let start, stop, stride =
    match args with
    | [ v ] -> 0, to_int v, 1
    | [ v1; v2 ] -> to_int v1, to_int v2, 1
    | [ v1; v2; s ] -> to_int v1, to_int v2, to_int s
    | _ -> errorf "range expects one, two, or three arguments"
  in
  if stride = 0 then errorf "range cannot use a stride of 0";
  let index = ref start in
  let next () =
    let v = !index in
    if (stride > 0 && v >= stop) || (stride < 0 && v <= stop)
    then None
    else (
      index := v + stride;
      Z.of_int v |> Bc_value.int |> Option.some)
  in
  Bc_value.iterator ~next

let len_fn _eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"len";
  let l =
    match (args : Bc_value.t list) with
    | [ Tuple l ] -> Array.length l
    | [ List l ] -> Queue.length l
    | [ Str s ] -> String.length s
    | [ Dict d ] -> Hashtbl.length d
    | [ v ] -> Bc_value.cannot_be_interpreted_as v "type with len"
    | _ -> errorf "len takes exactly one argument"
  in
  Z.of_int l |> Bc_value.int

let to_string eval_fn (v : Bc_value.t) ~escape_special_chars =
  let default_fn = Bc_value.to_string ~escape_special_chars in
  match v with
  | Object { cls = _; attrs } ->
    (match Hashtbl.find attrs "__str__" with
    | Some (Function fn) -> eval_fn fn [] [] |> Bc_value.str_exn
    | Some _ -> errorf "__str__ is not callable"
    | None -> default_fn v)
  | v -> default_fn v

let print_fn eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"print";
  List.map args ~f:(to_string eval_fn ~escape_special_chars:false)
  |> String.concat ~sep:" "
  |> Stdio.printf "%s\n";
  Bc_value.None

let delattr_fn _eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"delattr";
  match (args : Bc_value.t list) with
  | [ Object { attrs; _ }; Str key ] ->
    Hashtbl.remove attrs key;
    Bc_value.none
  | _ -> errorf "delattr takes exactly two arguments (object, str)"

let getattr_fn _eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"getattr";
  match (args : Bc_value.t list) with
  | [ Object { attrs; _ }; Str key ] ->
    (match Hashtbl.find attrs key with
    | Some value -> value
    | None -> errorf "object has no attribute %s" key)
  | [ Object { attrs; _ }; Str key; default ] ->
    Hashtbl.find attrs key |> Option.value ~default
  | _ ->
    errorf
      "getattr takes two arguments (object, str) or three arguments (object, str, \
       default)"

let hasattr_fn _eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"hasattr";
  match (args : Bc_value.t list) with
  | [ Object { attrs; _ }; Str key ] -> Bc_value.bool (Hashtbl.mem attrs key)
  | _ -> errorf "hasattr takes exactly two arguments (object, str)"

let setattr_fn _eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"setattr";
  match (args : Bc_value.t list) with
  | [ Object { attrs; _ }; Str key; data ] ->
    Hashtbl.set attrs ~key ~data;
    Bc_value.none
  | _ -> errorf "setattr takes exactly three arguments (object, str, value)"

let isinstance_fn _eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"isinstance";
  match (args : Bc_value.t list) with
  | [ Object { cls; _ }; v ] ->
    let rec loop : Bc_value.t -> bool = function
      | Tuple vs -> Array.exists vs ~f:loop
      | Class target_class -> Bc_value.is_subclass cls ~target_class
      | _ -> errorf "isinstance only accepts tuples or classes for its second argument"
    in
    Bc_value.bool (loop v)
  | _ -> errorf "isinstance takes exactly two arguments (object, class)"

let issubclass_fn _eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"issubclass";
  match (args : Bc_value.t list) with
  | [ Class cls; v ] ->
    let rec loop : Bc_value.t -> bool = function
      | Tuple vs -> Array.exists vs ~f:loop
      | Class target_class -> Bc_value.is_subclass cls ~target_class
      | _ -> errorf "issubclass only accepts tuples or classes for its second argument"
    in
    Bc_value.bool (loop v)
  | _ -> errorf "issubclass takes exactly two arguments (class1, class2)"

let str_fn eval_fn args kwargs =
  check_empty_kwargs kwargs ~name:"str";
  match (args : Bc_value.t list) with
  | [ v ] -> to_string eval_fn v ~escape_special_chars:false |> Bc_value.str
  | _ -> errorf "str takes exactly one argument"

let builtins () =
  List.map
    [ "print", print_fn
    ; "range", range_fn
    ; "len", len_fn
    ; "delattr", delattr_fn
    ; "getattr", getattr_fn
    ; "hasattr", hasattr_fn
    ; "setattr", setattr_fn
    ; "isinstance", isinstance_fn
    ; "issubclass", issubclass_fn
    ; "str", str_fn
    ]
    ~f:(fun (name, fn) -> name, Bc_value.Builtin_fn { name; fn })
  |> Bc_scope.of_alist_exn

type filename_and_lineno =
  { filename : string
  ; lineno : int option
  }
[@@deriving sexp]

type backtrace = filename_and_lineno list [@@deriving sexp]

exception Exn_with_backtrace of Exn.t * backtrace

let () =
  Caml.Printexc.register_printer (function
      | Exn_with_backtrace (exn, backtrace) ->
        let backtrace =
          List.mapi backtrace ~f:(fun index { filename; lineno } ->
              let lineno = Option.value_map lineno ~default:"unknown" ~f:Int.to_string in
              Printf.sprintf "%2d %s: line %s" (1 + index) filename lineno)
          |> String.concat ~sep:"\n"
        in
        Printf.sprintf "Backtrace:\n" ^ backtrace ^ "\n" ^ Exn.to_string exn
        |> Option.some
      | _ -> None)

let backtrace ~frames =
  Stack.fold frames ~init:[] ~f:(fun acc frame ->
      let filename, lineno = Bc_frame.current_filename_and_lineno frame in
      { filename; lineno } :: acc)

let eval_frame ~frame =
  let frames = Stack.create () in
  Stack.push frames frame;
  (* Avoid using recursion here so that the recursion depth can be controlled explicitely
     in the sys module.
  *)
  let continue = ref true in
  while !continue do
    match Stack.top frames with
    | None -> continue := false
    | Some frame ->
      let action =
        try Bc_frame.eval_step frame with
        | exn -> raise (Exn_with_backtrace (exn, backtrace ~frames))
      in
      (match action with
      | Continue -> ()
      | Call_fn { code; local_scope } ->
        let call_frame = Bc_frame.call_frame frame ~code ~local_scope in
        Stack.push frames call_frame
      | Return value ->
        let callee_frame = Stack.pop_exn frames in
        let callee_stack = Bc_frame.stack callee_frame in
        if not (Stack.is_empty callee_stack)
        then
          Printf.failwithf
            "non-empty stack upon return: %s"
            (Stack.sexp_of_t Bc_value.sexp_of_t callee_stack |> Sexp.to_string_hum)
            ();
        (match Bc_frame.parent_frame callee_frame with
        | None -> ()
        | Some caller_frame -> Bc_frame.function_call_returned caller_frame value))
  done

let eval code =
  let global_scope = Bc_scope.create () in
  let builtins = builtins () in
  let frame = Bc_frame.top_frame ~code ~global_scope ~builtins in
  eval_frame ~frame;
  let stack = Bc_frame.stack frame in
  if not (Stack.is_empty stack)
  then
    Printf.failwithf
      "non-empty final stack: %s"
      (Stack.sexp_of_t Bc_value.sexp_of_t stack |> Sexp.to_string_hum)
      ()

let () = Bc_frame.set_eval_frame eval_frame
