open Base
open Minipy

let%expect_test "syntax" =
  let ast =
    Basic_tests.parse_str
      {|

# here are some comments

x = 81 --- 39
print(x)

x = -++-+-+ 39 +81
print(x)

x = --- 39 +81
print(x)

x = 81 - 39
print(x)

x = 81-39
print(x)
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        ((Val_int 42))
        ((Val_int 42))
        ((Val_int 42))
        ((Val_int 42))
        ((Val_int 42))
      |}]
