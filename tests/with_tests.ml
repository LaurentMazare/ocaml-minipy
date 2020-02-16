open Base
open Minipy

let%expect_test "if" =
  let ast =
    Basic_tests.parse_str
      {|

class Context():
  def __init__(self, x):
    self._x = x

  def __enter__(self):
    print('entering', self._x)

  def __exit__(self, type, value, traceback):
    print('exiting', self._x, value)

with Context(42):
  print('inside')

try:
  with Context(42):
    print('inside1')
    raise BaseException
    print('inside2')
except BaseException:
  print('caught')
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        entering 42
        inside
        exiting 42
        entering 42
        inside1
        exiting 42
        caught
      |}]
