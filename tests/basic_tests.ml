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

let%expect_test _ =
  let ast = parse_str "x=1\nprint(42)\n" in
  Stdio.printf !"%{sexp:Ast.t}\n" ast;
  [%expect
    {|
        ((Assign (targets ((Name x))) (value (Num 1)))
         (Expr (value (Call (func (Name print)) (args ((Num 42)))))))
      |}]

let%expect_test _ =
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

let%expect_test _ =
  let ast = parse_str {|
def f(x, y):
  return x+y

print(f(1000, 337))
|} in
  Interpreter.simple_eval ast;
  [%expect {|
        ((Val_int 1337))
      |}]
