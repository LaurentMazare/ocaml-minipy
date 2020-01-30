open Base

let () =
  let mini_ast =
    Pyast.Parse.parse ~filename:"example/test_syntax.py" ~print_tokens:true
    |> Or_error.ok_exn
  in
  Pyast.Mini_ast.sexp_of_t mini_ast |> Sexp.to_string |> Stdio.printf "%s\n%!";
  Pyast.Mini_ast.simple_eval mini_ast
