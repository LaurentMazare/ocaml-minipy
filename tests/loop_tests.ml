open Base
open Minipy

let%expect_test "while" =
  let ast =
    Basic_tests.parse_str
      {|
# Some while loop tests
def fact1(n):
  res = 1
  while n > 0:
    res = res * n
    n = n - 1
  return res

def fact2(n):
  res = 1
  while True:
    res = res * n
    n = n - 1
    if n == 0: break
  return res

print(fact1(10), fact2(10))

def sum_even(n):
  k = 0
  res = 0
  while k < n:
    k = k + 1
    if k % 2 == 1:
      continue
    res = res + k
  return res

print(sum_even(10))
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        ((Val_int 3628800)(Val_int 3628800))
        ((Val_int 30))
      |}]
