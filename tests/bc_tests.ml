open Base
open Minipy

let debug = false

let code =
  { Bc_code.opcodes =
      [| Bc_code.Opcode.LOAD_NAME, 0; LOAD_CONST, 0; CALL_FUNCTION, 1; POP_TOP, 0 |]
      |> Array.map ~f:(fun (opcode, arg) -> { Bc_code.opcode; arg; lineno = 0 })
  ; consts = [| Bc_value.Str "foobar" |]
  ; varnames = [||]
  ; names = [| "print" |]
  ; filename = "test"
  }

let%expect_test "bytecode" =
  let () = Bc_eval.eval code in
  [%expect {| foobar |}]

let parse_compile_and_run str =
  let ast = Basic_tests.parse_str str in
  let code = Bc_compiler.compile ast in
  if debug then Stdio.printf "%s\n%!" (Bc_value.sexp_of_code code |> Sexp.to_string_hum);
  Bc_eval.eval code

let%expect_test "hello" =
  parse_compile_and_run "print(\"Hello World!\")\n";
  [%expect {| Hello World! |}];
  parse_compile_and_run "print(41+1)";
  [%expect {| 42 |}]

let%expect_test "fn" =
  parse_compile_and_run {|
def f(x, y):
  return x+y

print(f(1000, 337))
|};
  [%expect {| 1337 |}]

let%expect_test "fn2" =
  parse_compile_and_run
    {|
def f(x, y):
  return x*y if x else y

print(f(1000, 337))
print(f(0, 337))
|};
  [%expect {|
    337000
    337 |}]

let%expect_test "fact" =
  parse_compile_and_run
    {|
def fact(n):
  return n * fact(n-1) if n else 1

print(fact(10))
print(fact(100))
|};
  [%expect
    {|
    3628800
    93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000 |}]

let%expect_test "vars" =
  parse_compile_and_run
    {|
x = 4
while x:
  x = x - 1
  print(x)

print('there')
x = 4
while x:
  x = x - 1
  print(x)
else: print('else')

print('there')
|};
  [%expect
    {|
    3
    2
    1
    0
    there
    3
    2
    1
    0
    else
    there |}]

let%expect_test "bool-expr" =
  parse_compile_and_run
    {|
b1 = True
b2 = False
b3 = not True
b4 = not False
b5 = not not b4
b6 = not (0 == 0)
b7 = not (True and False)
b8 = True and False or True
b9 = True and (False or True)
b10 = not False or True
b11 = not (False or True)
print(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)

print(1 <= 2 >= 3, 1 <= 2 >= 2, 1. < 1.1 < 1.05, 1. < 1.05 < 1.1 < 1.15)

print('1 <= 2', 1 <= 2)
print('2 <= 2', 2 <= 2)
print('2 <= 1', 2 <= 1)
print('2 < 2', 2 < 2)
print('2 > 2', 2 > 2)
print('2 >= 2', 2 >= 2)
print('2 == 2', 2 == 2)
print('2 != 2', 2 != 2)

def true_p():
  print('true_p')
  return True

print(True and true_p())
print(False and true_p())
print(True or true_p())
print(False or true_p())
|};
  [%expect
    {|
        True False False True True False True True True True False
        False True False True
        1 <= 2 True
        2 <= 2 True
        2 <= 1 False
        2 < 2 False
        2 > 2 False
        2 >= 2 True
        2 == 2 True
        2 != 2 False
        true_p
        True
        False
        True
        true_p
        True
      |}]

let%expect_test "while-continue-break" =
  parse_compile_and_run
    {|
x = 0
while x < 5:
  x = x + 1
  if x % 2 == 0: continue
  print(x)
else: print('else')

x = 0
while True:
  x += 1
  if x % 2 == 0: continue
  print(x)
  if x > 4: break
else: print('else')
|};
  [%expect
    {|
        1
        3
        5
        else
        1
        3
        5
      |}]

let%expect_test "tuples-lists" =
  parse_compile_and_run
    {|
x = 1, 2
x = x, 3
print(x)

[(a, b), c] = x
print(a * 100 + b * 10 + c)
|};
  [%expect {|
        ((1, 2), 3)
        123
      |}]

let%expect_test "sieve" =
  parse_compile_and_run
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
|};
  [%expect {| [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37] |}]

let%expect_test "list-compr" =
  parse_compile_and_run
    {|
l = [i*i for i in range(4)]
print(l)

l = [i*i for i in range(5) if i != 3 ]
print(l)

l = [(i, j) for i in range(5) for j in range(i)]
print(l)

l = [(i, j) for i in range(5) for j in range(i) if i % 2 == 0]
print(l)

l = [(i, j) for i in range(5) if i % 2 == 0 for j in range(i+1) if i == j]
print(l)
l.append(1)
|};
  [%expect
    {|
    [0, 1, 4, 9]
    [0, 1, 4, 16]
    [(1, 0), (2, 0), (2, 1), (3, 0), (3, 1), (3, 2), (4, 0), (4, 1), (4, 2), (4, 3)]
    [(2, 0), (2, 1), (4, 0), (4, 1), (4, 2), (4, 3)]
    [(0, 0), (2, 2), (4, 4)] |}]

let%expect_test "kwargs" =
  parse_compile_and_run
    {|
def f(x, y):
  return x-y
print(f(1, 2))
print(f(x=1, y=2))
print(f(y=1, x=2))
|};
  [%expect {|
        -1
        -1
        1
      |}]

let%expect_test "bc-fn" =
  parse_compile_and_run
    {|
################
# Function tests
################

def f(a, b): return a-b

print(f(1, 0), f(0, 1), f(a=1, b=0), f(a=0, b=1), f(b=1, a=0), f(b=0, a=1), f(43, b=1))

def f(a, b, c, *args):
  print(a+b+c, len(args))

f(1, 2, 3)
f(1, 2, 3, 4)
f(1, 2, 3, 4, 5)

def sum1(a, b=1): return a+2*b
print(sum1(40), sum1(42, b=0), sum1(b=20, a=2))

def sum(a, b="foo"):
  return a + b
print(sum(1, b=41), sum("bar"), sum(b="bar", a="foo"))

def all_args(a, *arg, b="42", **kwarg):
  return kwarg[a + b], arg

print(all_args("a", a42="foo"))
print(all_args("a", "b", "c", a42="foo"))
print(all_args("a", "b", "c", a43="bar", b="43", a42="foo"))
|};
  [%expect
    {|
        1 -1 1 -1 -1 1 42
        6 0
        6 1
        6 2
        42 42 42
        42 barfoo foobar
        ('foo', [])
        ('foo', ['b', 'c'])
        ('bar', ['b', 'c'])
      |}]

let%expect_test "bc-nested-fn" =
  parse_compile_and_run
    {|
def prime_dec(n):
  factors = [i for i in range(n+1)]
  for p in range(2, 1+n):
    if factors[p] == p:
      for q in range(p, 1+n, p): factors[q] = p
  def pd(m):
    pqs = []
    while m != 1:
      p, q = factors[m], 0
      while m % p == 0:
        m //= p
        q += 1
      pqs.append((p, q)) # Use append!!!
    return pqs
  return pd

pd = prime_dec(2000)
print(pd(100), pd(1337), pd(314))
pd = prime_dec(5000)
print(pd(2500))
|};
  [%expect
    {|
        [(5, 2), (2, 2)] [(191, 1), (7, 1)] [(157, 1), (2, 1)]
        [(5, 4), (2, 2)]
      |}]

let%expect_test "class" =
  parse_compile_and_run
    {|
class A:
  def set_a(self, a):
    self._a = a

  def twice_a(self):
    return 2 * self._a

a = A()
print(a)
a.set_a(21)
print(a.twice_a())

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

|};
  [%expect {|
    <object.A>
    42
    45.14159265358979
    foobar |}]

let%expect_test "fn" =
  parse_compile_and_run
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
|};
  [%expect
    {|
        43
        44
        True False True True
        False True True True
      |}]

let%expect_test "fn" =
  parse_compile_and_run
    {|
class A():
  def __init__(self, x):
    self._x = x

  def __str__(self):
    return 'foo<' + str(self._x) + '>'

a = A(42)
print(a)
print(str(a))
|};
  [%expect {|
  |}]
