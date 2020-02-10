open Base
open Minipy

let%expect_test "fn" =
  let ast =
    Basic_tests.parse_str
      {|
class A():
  foo = 1

  def add_foo(self, x):
    return self.foo + x

a = A()
print(a, a.foo)
a.foo += 41
print(a, a.foo)
print(a.add_foo(1337 - 42))
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        <object A> 1
        <object A> 42
        1337
      |}]
