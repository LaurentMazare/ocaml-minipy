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

def double_loop(n):
  k = 0
  res = 0
  while k < n:
    k += 1
    while True:
      k += 1
      if k % 2 == 0: continue
      if k % 5 == 0: break
    res += k
  return res

print(double_loop(100))
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        ((Val_int 3628800)(Val_int 3628800))
        ((Val_int 30))
        ((Val_int 605))
      |}]

let%expect_test "for" =
  let ast =
    Basic_tests.parse_str
      {|
# Some for loop tests

res1, res2, res3, res4 = 1, 0, 0, 0
for i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
  res1 *= i
  if i % 2 == 0: continue
  elif i % 3 == 0: res2 = res2 + i
  else: res3 = res3 + i
  res4 = res4 + i
print(res1, res2, res3, res4)
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        ((Val_int 3628800)(Val_int 12)(Val_int 13)(Val_int 25))
      |}]
