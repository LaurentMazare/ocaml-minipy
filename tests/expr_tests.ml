open Base
open Minipy

let%expect_test "expr" =
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
x = 1+2*3-5-6+7*(8-9)+12*2*-1*3-(1+2+2*2)
print(x)
x = 1/(1+2/4)*3-5*(-5*(2-1)+6/2/2)
print(x)
x=1/2/3/4*5*7-9+12-1-2-3-3*3-3*-3+7*(((1/(1+0.5))))
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
        ((Val_int -90))
        ((Val_float 19.5))
        ((Val_float 3.1249999999999991))
      |}]
