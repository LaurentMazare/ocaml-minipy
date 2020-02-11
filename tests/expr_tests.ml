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
        42
        42
        42
        42
        42
        -90
        19.5
        3.1249999999999991
      |}]

let%expect_test "lambdas" =
  let ast =
    Basic_tests.parse_str
      {|

# here are some comments

fn = lambda x: x + 1
print(fn(0), fn(1))

fn0 = lambda: 42
fn_add = lambda x, y: x + y
print(fn0(), fn_add(3.14159265358979, 2.71828182846))
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        1 2
        42 5.85987448204979
      |}]

let%expect_test "parenthesis" =
  let ast =
    Basic_tests.parse_str
      {|
x = (
1)
y = (
  1)
y = (

  1
+ 2
        -y)
print(x, y)

xs = [
  i * i
  for i in range(100)
  if i > 45
and i < 48
]
print(xs)

xs = [

  (1,
2

       ),
  "foo"
+      "BAR"]
print(xs)

x = x + \
1
x = x \
    + 1
print(x)
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        1 2
        [2116, 2209]
        [(1, 2), 'fooBAR']
        3
      |}]

let%expect_test "bool-expr" =
  let ast =
    Basic_tests.parse_str
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
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        True False False True True False True True True True False
      |}]
