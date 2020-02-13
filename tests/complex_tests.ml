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
        3.14
        2.1400000000000006
        5.2800000000000011
        41
        42
        43
        44
        45
        46
        47
        48
        49
        50
        52
        9.8636014000893688
        9.8695444007894544
        6
        yay1
        yay3
        2
        nay1
      |}]

let%expect_test "sieve" =
  let ast =
    Basic_tests.parse_str
      {|
# A simple prime sieve.
def sieve(maxp):
  is_prime = [True] * (1+maxp)
  primes = []
  for p in range(2, 1+maxp):
    if is_prime[p]:
      primes = primes + [p] # Append would be better...
      for q in range(p, 1+maxp, p): is_prime[q] = False
  return primes

print(sieve(40))
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
      |}]
