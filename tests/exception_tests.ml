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
  [%expect
    {|
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

let%expect_test "exception-cls" =
  let ast =
    Basic_tests.parse_str
      {|
class A(BaseException):
  pass

class B(A):
  pass

class C(BaseException):
  pass

def f(e):
  try:
    raise e
  except A: print('raised A')
  except: print('raised something else')
  print('done')

f(A())
f(B())
f(C())

def f(e):
  try:
    try:
      raise e
    except B: print('caught B')
    else: print('no B')
  except A: print('caught A')
  else: print('no A')

print('trying A')
f(A())
print('trying B')
f(B())
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        raised A
        done
        raised A
        done
        raised something else
        done
        trying A
        caught A
        trying B
        caught B
        no A
      |}]
