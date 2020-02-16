open Base
open Minipy

let%expect_test "fn" =
  let ast =
    Basic_tests.parse_str
      {|
x = int('42')
x += 1
print(x, str(x) + ' foobar')
print(bool(123), bool(0), bool(''), bool('T'))

def fact(n): return n*fact(n-1) if n else 1
print(float(fact(100)))

class A():
  def __init__(self, x):
    self._x = x

  def __str__(self):
    return 'foo<' + str(self._x) + '>'

a = A(42)
print(a)
print(str(a))
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        43 43 foobar
        True False False True
        9.3326215443944151e+157
        foo<42>
        foo<42>
      |}]
