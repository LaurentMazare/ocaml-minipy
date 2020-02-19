open Base
open Minipy

let code =
  { Bc_frame.Code.opcodes = [| LOAD_CONST, 0 |]
  ; consts = [| Bc_value.Str "foobar" |]
  ; varnames = [||]
  ; names = [||]
  }

let%expect_test "bytecode" =
  let eval = Bc_eval.create () in
  let () = Bc_eval.eval eval code in
  [%expect {| |}]
