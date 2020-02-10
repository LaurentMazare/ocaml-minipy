open Base
open Minipy

let%expect_test "fn" =
  let ast =
    Basic_tests.parse_str
      {|
class A():
  foo = 1

a = A()
print(a, a.foo)
a.foo += 41
print(a, a.foo)
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        [(5, 2), (2, 2)] [(191, 1), (7, 1)] [(157, 1), (2, 1)]
        [(5, 4), (2, 2)]
      |}]
