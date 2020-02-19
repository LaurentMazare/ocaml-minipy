open Base

let builtins () =
  (* TODO: add some builtins. *)
  Bc_scope.create ()

type t =
  { frames : Bc_frame.t Stack.t
  ; global_scope : Bc_scope.t
  ; builtins : Bc_scope.t
  }

let create () =
  { frames = Stack.create (); global_scope = Bc_scope.create (); builtins = builtins () }

let eval t code =
  let frame = Bc_frame.create ~code ~global_scope:t.global_scope ~builtins:t.builtins in
  ignore (Bc_frame.eval frame : Bc_value.t option)
