open Base
open Minipy

let filename = "examples/test_syntax.py"

let () =
  let tokens = Parse.tokens_file filename in
  List.iter tokens ~f:(fun token ->
      Parse.token_to_string token |> Stdio.printf "token: <%s>\n%!");
  let mini_ast = Parse.parse_file filename |> Parse.ok_exn in
  Ast.sexp_of_t mini_ast |> Sexp.to_string |> Stdio.printf "%s\n%!";
  Interpreter.simple_eval mini_ast
