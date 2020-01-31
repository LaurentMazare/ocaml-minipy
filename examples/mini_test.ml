open Base
open Minipy

let () =
  let mini_ast = Parse.parse ~filename:"examples/test_syntax.py" |> Parse.ok_exn in
  Ast.sexp_of_t mini_ast |> Sexp.to_string |> Stdio.printf "%s\n%!";
  Interpreter.simple_eval mini_ast
