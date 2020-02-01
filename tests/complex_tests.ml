open Base
open Minipy

let%expect_test "syntax" =
  let ast =
    Basic_tests.parse_str
      {|

# here are some comments

x = 3.14
y = 3.14 - 1 + 2 - 2
print(x) #test
print(y)
print(x+y)

def fn(x, y):
  while x != 50:
    x = x + 1
    print(x)
  return x + y

print(fn(40, 2))

def approx_pi2(n):
  k = 1
  res = 0.0
  while k != n:
    res = res + 1 / (k * k)
    k = k + 1
  return res * 6

print(approx_pi2(1000))
print(approx_pi2(100000))

x = 3
def fff(x, y):
  if x == y: print(x+y)
  else: print(x)

  if x == y:
    if x == 3:
      if True:
        print("yay1")
  else: print("nay1")
  if x == y:
    if x == 3:
      if True:
        print("yay3")
    else: print("nay3")

fff(3, 3)
fff(2, 3)
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        ((Val_float 3.14))
        ((Val_float 2.14))
        ((Val_float 5.28))
        ((Val_int 41))
        ((Val_int 42))
        ((Val_int 43))
        ((Val_int 44))
        ((Val_int 45))
        ((Val_int 46))
        ((Val_int 47))
        ((Val_int 48))
        ((Val_int 49))
        ((Val_int 50))
        ((Val_int 52))
        ((Val_float 9.8636014000893688))
        ((Val_float 9.8695444007894544))
        ((Val_int 6))
        ((Val_str yay1))
        ((Val_str yay3))
        ((Val_int 2))
        ((Val_str nay1))
      |}]
