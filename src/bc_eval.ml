open Base

let builtins () =
  let print_fn args =
    List.map args ~f:(Bc_value.to_string ~escape_special_chars:false)
    |> String.concat ~sep:" "
    |> Stdio.printf "%s\n";
    Bc_value.None
  in
  List.map [ "print", print_fn ] ~f:(fun (name, fn) ->
      name, Bc_value.Builtin_fn { name; fn })
  |> Bc_scope.of_alist_exn

type t =
  { frames : Bc_frame.t Stack.t
  ; global_scope : Bc_scope.t
  ; builtins : Bc_scope.t
  }

let create () =
  { frames = Stack.create (); global_scope = Bc_scope.create (); builtins = builtins () }

let eval code =
  let t = create () in
  let global_scope = t.global_scope in
  let builtins = t.builtins in
  let frame = Bc_frame.top_frame ~code ~global_scope ~builtins in
  Stack.push t.frames frame;
  (* Avoid using recursion here so that the recursion depth can be controlled explicitely
     in the sys module.
  *)
  let continue = ref true in
  while !continue do
    match Stack.top t.frames with
    | None -> continue := false
    | Some frame ->
      (match Bc_frame.eval_step frame with
      | Continue -> ()
      | Call_fn { code; local_scope } ->
        let call_frame = Bc_frame.call_frame frame ~code ~local_scope in
        Stack.push t.frames call_frame
      | Return value ->
        let callee_frame = Stack.pop_exn t.frames in
        let callee_stack = Bc_frame.stack callee_frame in
        if not (Stack.is_empty callee_stack)
        then
          Printf.failwithf
            "non-empty stack upon return: %s"
            (Stack.sexp_of_t Bc_value.sexp_of_t callee_stack |> Sexp.to_string_hum)
            ();
        Stack.top t.frames
        |> Option.iter ~f:(fun caller_frame ->
               Bc_frame.function_call_returned caller_frame value))
  done;
  let stack = Bc_frame.stack frame in
  if not (Stack.is_empty stack)
  then
    Printf.failwithf
      "non-empty final stack: %s"
      (Stack.sexp_of_t Bc_value.sexp_of_t stack |> Sexp.to_string_hum)
      ()
