open Base

let () =
  let mini_ast = Pyast.Parse.parse ~filename:"/tmp/test.py" |> Or_error.ok_exn in
  Pyast.Mini_ast.sexp_of_t mini_ast |> Sexp.to_string |> Stdio.printf "%s\n%!"
