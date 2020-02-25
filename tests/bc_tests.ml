open Base
open Minipy

let debug = false

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
  if debug then Stdio.printf "%s\n%!" (Bc_value.sexp_of_code code |> Sexp.to_string_hum);
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
  [%expect
    {|
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

let%expect_test "bool-expr" =
  parse_compile_and_run
    {|
b1 = True
b2 = False
b3 = not True
b4 = not False
b5 = not not b4
b6 = not (0 == 0)
b7 = not (True and False)
b8 = True and False or True
b9 = True and (False or True)
b10 = not False or True
b11 = not (False or True)
print(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)

print(1 <= 2 >= 3, 1 <= 2 >= 2, 1. < 1.1 < 1.05, 1. < 1.05 < 1.1 < 1.15)

print('1 <= 2', 1 <= 2)
print('2 <= 2', 2 <= 2)
print('2 <= 1', 2 <= 1)
print('2 < 2', 2 < 2)
print('2 > 2', 2 > 2)
print('2 >= 2', 2 >= 2)
print('2 == 2', 2 == 2)
print('2 != 2', 2 != 2)

def true_p():
  print('true_p')
  return True

print(True and true_p())
print(False and true_p())
print(True or true_p())
print(False or true_p())
|};
  [%expect
    {|
        True False False True True False True True True True False
        False True False True
        1 <= 2 True
        2 <= 2 True
        2 <= 1 False
        2 < 2 False
        2 > 2 False
        2 >= 2 True
        2 == 2 True
        2 != 2 False
        true_p
        True
        False
        True
        true_p
        True
      |}]
