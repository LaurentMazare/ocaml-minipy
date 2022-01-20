open Base
open Minipy

let%expect_test "tuple/list" =
  let ast =
    Basic_tests.parse_str
      {|
x = 1,
print(x)

x = (1,), 2,
print(x)

x = 1,2,"foobar",'barfoo',1+2,
print(x)

x = [1, 2, 3],
print(x)
x = [1, 2, 3],2,3,
print(x[2], x[-1], x[0][-2])

x = [1, 2, 3]
x[0*5+1] = 4
print(x)
x[-1] = [1, 2]
print(x)
x[-1][-1] = [1, 2]
print(x)

x, y, z = "foo", "bar", (1, 2, 3)
print(z, x, y)
x, (y, z) = "bar", ("foo", (3, 2, 1))
print(z, x, y)

x=y=a,b=42, 3.141592
print(x, y, a, b)
x, (y, z)=(a, b), c = (4, 5), ["foo", "bar"]
print((x, y, z), (a, b, c))
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        (1,)
        ((1,), 2)
        (1, 2, 'foobar', 'barfoo', 3)
        ([1, 2, 3],)
        3 3 2
        [1, 4, 3]
        [1, 4, [1, 2]]
        [1, 4, [1, [1, 2]]]
        (1, 2, 3) foo bar
        (3, 2, 1) bar foo
        (42, 3.141592) (42, 3.141592) 42 3.141592
        ((4, 5), 'foo', 'bar') (4, 5, ['foo', 'bar'])
      |}]

let%expect_test "list comp" =
  let ast =
    Basic_tests.parse_str
      {|
l = [x*x for x in range(5)]
print(l)
l = [(x, y) for x in range(3) for y in range(x)]
print(l)
l = [x*2 for x in range(5) if x % 2 == 1]
print(l)
l = [(x, y) for x in range(3) for y in range(x+3) if x == y]
print(l)
|}
  in
  Interpreter.simple_eval ast;
  [%expect
    {|
        [0, 1, 4, 9, 16]
        [(1, 0), (2, 0), (2, 1)]
        [2, 6]
        [(0, 0), (1, 1), (2, 2)]
      |}]

let%expect_test "list del" =
  let ast =
    Basic_tests.parse_str
      {|
l = [x*x for x in range(5)]
print(l)
del l[2]
print(l)
del l[-1]
print(l)
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        [0, 1, 4, 9, 16]
        [0, 1, 9, 16]
        [0, 1, 9]
      |}]
