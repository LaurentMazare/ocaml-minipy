open Base
open Minipy

let%expect_test "fn" =
  let ast =
    Basic_tests.parse_str
      {|
################
# Function tests
################

def f(a, b): return a-b

print(f(1, 0), f(0, 1), f(a=1, b=0), f(a=0, b=1), f(b=1, a=0), f(b=0, a=1), f(43, b=1))

def f(a, b, c, *args):
  print(a+b+c, len(args))

f(1, 2, 3)
f(1, 2, 3, 4)
f(1, 2, 3, 4, 5)

def sum1(a, b=1): return a+2*b
print(sum1(40), sum1(42, b=0), sum1(b=20, a=2))

def sum(a, b="foo"):
  return a + b
print(sum(1, b=41), sum("bar"), sum(b="bar", a="foo"))

def all_args(a, *arg, b="42", **kwarg):
  return kwarg[a + b], arg

print(all_args("a", a42="foo"))
print(all_args("a", "b", "c", a42="foo"))
print(all_args("a", "b", "c", a43="bar", b="43", a42="foo"))
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        1 -1 1 -1 -1 1 42
        6 0
        6 1
        6 2
        42 42 42
        42 barfoo foobar
        ('foo', [])
        ('foo', ['b', 'c'])
        ('bar', ['b', 'c'])
      |}]
