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
  [%expect
    {|
        ((Val_int 1)(Val_int 2))
        ((Val_int 42)(Val_float 5.85987448204979))
      |}]

let%expect_test "dict" =
  let ast =
    Basic_tests.parse_str
      {|
# Dictionary tests
d = { "key": (1, 2), "c": 299792458, 42: 1337 }
d["foo"] = (1, "bar")
print(d["c"], d["foo"])

def set(dd, key, value): dd[key] = value

set(d, (1, 2), "foobar")
print(d[(1, 2)])
set(d, (1, 2), "barfoo")
print(d[1, 2])
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        ((Val_int 299792458)(Val_tuple((Val_int 1)(Val_str bar))))
        ((Val_str foobar))
        ((Val_str barfoo))
      |}]
