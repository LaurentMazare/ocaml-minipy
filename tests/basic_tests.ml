open Base
open Minipy

let debug = false

let parse_str str =
  let str = str ^ "\n" in
  if debug
  then (
    let tokens = Parse.tokens_string str in
    Stdio.printf
      "token: %s\n%!"
      (List.map tokens ~f:Parse.token_to_string |> String.concat ~sep:"\n"));
  Parse.parse_string str |> Parse.ok_exn

let%expect_test "parse" =
  let ast = parse_str "x=1\nprint(42)\n" in
  Stdio.printf !"%{sexp:Ast.t}\n" ast;
  [%expect
    {|
        ((Assign (targets ((Name x))) (value (Num 1)))
         (Expr (value (Call (func (Name print)) (args ((Num 42))) (keywords ())))))
      |}]

let%expect_test "hello" =
  let ast = parse_str "print(\"Hello World!\")\n" in
  Interpreter.simple_eval ast;
  [%expect {|
        Hello World!
      |}];
  let ast = parse_str "print(41+1)" in
  Interpreter.simple_eval ast;
  [%expect {|
        42
      |}]

let%expect_test "fn" =
  let ast = parse_str {|
def f(x, y):
  return x+y

print(f(1000, 337))
|} in
  Interpreter.simple_eval ast;
  [%expect {|
        1337
      |}]

let%expect_test "builtins" =
  let ast = parse_str {|
print(fact(10))
|} in
  let print_fn _ args _ =
    match (args : Value.t list) with
    | [ Val_int i ] ->
      Stdio.printf "%s\n" (Z.to_string i);
      Value.none
    | _ ->
      Stdio.printf "unsupported\n";
      Val_none
  in
  let rec fact = function
    | 0 -> 1
    | n -> n * fact (n - 1)
  in
  let fact_fn _ args _ =
    match (args : Value.t list) with
    | [ Val_int i ] -> Value.Val_int (Z.to_int i |> fact |> Z.of_int)
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
        [1000, 337, 1337]
      |}]

let%expect_test "semicolon" =
  let ast =
    parse_str
      {|
x=1;
print(x)
x=1 ; y=2
print(x,y)
x=1 ; y=2;y=3;
print(x,y)
def f(x, y):
  x=x+3;y=y
  x=x+3;y=y;
  x=x+3;
  return [x, y, x+y]

print(f(991, 337))
|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        1
        1 2
        1 3
        [1000, 337, 1337]
      |}]
