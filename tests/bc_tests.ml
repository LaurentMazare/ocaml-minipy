open Base
open Minipy

let code =
  { Bc_code.opcodes = [| LOAD_NAME, 0; LOAD_CONST, 0; CALL_FUNCTION, 1; POP_TOP, 0 |]
  ; consts = [| Bc_value.Str "foobar" |]
  ; varnames = [||]
  ; names = [| "print" |]
  }

let%expect_test "bytecode" =
  let () = Bc_eval.eval code in
  [%expect {| foobar |}]

let parse_compile_and_run str =
  let ast = Basic_tests.parse_str str in
  let code = Bc_compiler.compile ast in
  Bc_eval.eval code

let%expect_test "hello" =
  parse_compile_and_run "print(\"Hello World!\")\n";
  [%expect {| Hello World! |}];
  parse_compile_and_run "print(41+1)";
  [%expect {| 42 |}]

let%expect_test "fn" =
  parse_compile_and_run {|
def f(x, y):
  return x+y

print(f(1000, 337))
|};
  [%expect {| 1337 |}]
