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
        ((Val_tuple((Val_int 1))))
        ((Val_tuple((Val_tuple((Val_int 1)))(Val_int 2))))
        ((Val_tuple((Val_int 1)(Val_int 2)(Val_str foobar)(Val_str barfoo)(Val_int 3))))
        ((Val_tuple((Val_list((Val_int 1)(Val_int 2)(Val_int 3))))))
        ((Val_int 3)(Val_int 3)(Val_int 2))
        ((Val_list((Val_int 1)(Val_int 4)(Val_int 3))))
        ((Val_list((Val_int 1)(Val_int 4)(Val_list((Val_int 1)(Val_int 2))))))
        ((Val_list((Val_int 1)(Val_int 4)(Val_list((Val_int 1)(Val_list((Val_int 1)(Val_int 2))))))))
        ((Val_tuple((Val_int 1)(Val_int 2)(Val_int 3)))(Val_str foo)(Val_str bar))
        ((Val_tuple((Val_int 3)(Val_int 2)(Val_int 1)))(Val_str bar)(Val_str foo))
        ((Val_tuple((Val_int 42)(Val_float 3.141592)))(Val_tuple((Val_int 42)(Val_float 3.141592)))(Val_int 42)(Val_float 3.141592))
        ((Val_tuple((Val_tuple((Val_int 4)(Val_int 5)))(Val_str foo)(Val_str bar)))(Val_tuple((Val_int 4)(Val_int 5)(Val_list((Val_str foo)(Val_str bar))))))
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
        ((Val_list((Val_int 0)(Val_int 1)(Val_int 4)(Val_int 9)(Val_int 16))))
        ((Val_list((Val_tuple((Val_int 1)(Val_int 0)))(Val_tuple((Val_int 2)(Val_int 0)))(Val_tuple((Val_int 2)(Val_int 1))))))
        ((Val_list((Val_int 2)(Val_int 6))))
        ((Val_list((Val_tuple((Val_int 0)(Val_int 0)))(Val_tuple((Val_int 1)(Val_int 1)))(Val_tuple((Val_int 2)(Val_int 2))))))
      |}]
