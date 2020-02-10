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
        <object A> 1
        <object A> 42
      |}]
