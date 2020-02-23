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

let%expect_test "fn2" =
  parse_compile_and_run
    {|
def f(x, y):
  return x*y if x else y

print(f(1000, 337))
print(f(0, 337))
|};
  [%expect {|
    337000
    337 |}]

let%expect_test "fact" =
  parse_compile_and_run
    {|
def fact(n):
  return n * fact(n-1) if n else 1

print(fact(10))
print(fact(100))
|};
  [%expect
    {|
    3628800
    93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000 |}]

let%expect_test "vars" =
  parse_compile_and_run
    {|
x = 4
while x:
  x = x - 1
  print(x)

print('there')
x = 4
while x:
  x = x - 1
  print(x)
else: print('else')

print('there')
|};
  [%expect {|
    3
    2
    1
    0
    there
    3
    2
    1
    0
    else
    there |}]
