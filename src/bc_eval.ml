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
  let frame = Bc_frame.create ~code ~global_scope:t.global_scope ~builtins:t.builtins in
  Stack.push t.frames frame;
  (* Avoid using recursion here so that the recursion depth can be controlled explicitely
     in the sys module.
  *)
  while not (Stack.is_empty t.frames) do
    let frame = Stack.pop_exn t.frames in
    let continue = ref true in
    while !continue do
      match Bc_frame.eval_step frame with
      | No_action -> ()
      | End_of_code -> continue := false
    done
  done
