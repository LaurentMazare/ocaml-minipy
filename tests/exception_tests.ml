open Base
open Minipy

let%expect_test "exception" =
  let ast =
    Basic_tests.parse_str
      {|
try:
  print('before error')
  x += 1
  print('after error')
except:
  print('raised')

def go(x):
  try:
    print('before error', x)
    x += 1
    print('after error', x)
  except:
    print('raised', x)
  else:
    print('else', x)
  finally:
    print('finally', x)
go('foo')
go(123)
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        before error
        raised
        before error foo
        raised foo
        finally foo
        before error 123
        after error 124
        else 124
        finally 124
      |}]
