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

let%expect_test "init" =
  let ast =
    Basic_tests.parse_str
      {|
class MyClass():
  def __init__(self, value):
    self._foo = 42
    self._bar = value

  def foobar(self): return self._foo + self._bar

  def set_foo(self, v):
    self._foo = v

  def set_bar(self, v):
    self._bar = v

myobj = MyClass(3.14159265358979)
print(myobj.foobar())
myobj.set_foo("foo")
myobj.set_bar("bar")
print(myobj.foobar())
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        45.14159265358979
        foobar
      |}]

let%expect_test "fn" =
  let ast =
    Basic_tests.parse_str
      {|
class A():
  def add(self, x):
    return x + 1

  def print_add(self, x):
    print(self.add(x))

class B(A):
  def add(self, x):
    return x + 2

a = A()
b = B()
a.print_add(42)
b.print_add(42)
print(isinstance(a, A), isinstance(a, B), isinstance(b, A), isinstance(b, B))
print(issubclass(A, B), issubclass(B, A), issubclass(A, A), issubclass(B, B))
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        43
        44
        True False True True
        False True True True
      |}]
