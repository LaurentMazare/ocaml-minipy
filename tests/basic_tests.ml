open Base
open Minipy

let debug = false

let parse_str data =
  let tmp_file = Caml.Filename.temp_file "minitest" ".py" in
  Exn.protect
    ~f:(fun () ->
      Stdio.Out_channel.write_all tmp_file ~data:(data ^ "\n");
      if debug
      then (
        let tokens = Parse.tokens_file tmp_file in
        Stdio.printf
          "token: %s\n%!"
          (List.map tokens ~f:Parse.token_to_string |> String.concat ~sep:"\n"));
      Parse.parse_file tmp_file |> Parse.ok_exn)
    ~finally:(fun () -> Caml.Sys.remove tmp_file)

let%expect_test "parse" =
  let ast = parse_str "x=1\nprint(42)\n" in
  Stdio.printf !"%{sexp:Ast.t}\n" ast;
  [%expect
    {|
        ((Assign (targets ((Name x))) (value (Num 1)))
         (Expr (value (Call (func (Name print)) (args ((Num 42)))))))
      |}]

let%expect_test "hello" =
  let ast = parse_str "print(\"Hello World!\")\n" in
  Interpreter.simple_eval ast;
  [%expect {|
        ((Val_str"Hello World!"))
      |}];
  let ast = parse_str "print(41+1)" in
  Interpreter.simple_eval ast;
  [%expect {|
        ((Val_int 42))
      |}]

let%expect_test "fn" =
  let ast = parse_str {|
def f(x, y):
  return x+y

print(f(1000, 337))
|} in
  Interpreter.simple_eval ast;
  [%expect {|
        ((Val_int 1337))
      |}]

let%expect_test "builtins" =
  let ast = parse_str {|
print(fact(10))
|} in
  let print_fn = function
    | [ Interpreter.Val_int i ] ->
      Stdio.printf "%d\n" i;
      Interpreter.Val_none
    | _ ->
      Stdio.printf "unsupported\n";
      Val_none
  in
  let rec fact = function
    | 0 -> 1
    | n -> n * fact (n - 1)
  in
  let fact_fn = function
    | [ Interpreter.Val_int i ] -> Interpreter.Val_int (fact i)
    | _ -> Val_none
  in
  let builtins =
    [ "print", print_fn; "fact", fact_fn ] |> Map.of_alist_exn (module String)
  in
  Interpreter.simple_eval ~builtins ast;
  [%expect {|
        3628800
      |}]

let%expect_test "fn" =
  let ast = parse_str {|
def f(x, y):
  return [x, y, x+y]

print(f(1000, 337))
|} in
  Interpreter.simple_eval ast;
  [%expect {|
        ((Val_list((Val_int 1000)(Val_int 337)(Val_int 1337))))
      |}]

let%expect_test "if" =
  let ast =
    parse_str
      {|

x = 1
if x == 0: print("foo")
if x == 1: print("bar1")

if x == 0:
  print("foo")

if x == 1:
  print("bar2")

if x == 0: print("foo")
else: print("bar3")

if x == 0: print("foo")
else:
  print("bar4")

if x == 0:
  print("foo")
elif x == 1:
  print("bar")
else:
  print("foobar")

if x == 0: print("foo")
elif x == 2: print("bar")
else: print("foobar")

x = x + 1
if x == 0: print("foo")
elif x == 1:
  print("nooooo")

  print("nooooo")
elif x == 2:
  print("bar")
  print("barbar")
else: print("foobar")

if x == 0: print("foo")
elif x == 1:
  print("nooooo")

  print("nooooo")
elif x == 3:

  print("nooooo")

if x == 2: print("barX")
elif x == 1:
  print("nooooo")

  print("nooooo")
elif x == 3:

  print("nooooo")

if x == 0: print("nope")
elif x == 2:
  print("bar")

  print("Bar")
elif x == 2:

  print("nooooo")


|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        ((Val_str bar1))
        ((Val_str bar2))
        ((Val_str bar3))
        ((Val_str bar4))
        ((Val_str bar))
        ((Val_str foobar))
        ((Val_str bar))
        ((Val_str barbar))
        ((Val_str barX))
        ((Val_str bar))
        ((Val_str Bar))
      |}]

let%expect_test "syntax" =
  let ast =
    parse_str
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
        ((Val_float 6.1400000000000006))
        ((Val_float 9.2800000000000011))
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
