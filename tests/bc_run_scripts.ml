open Base
open Minipy

let script_dir = "scripts"

let run_one filename =
  let ast = Parse.parse_file filename |> Parse.ok_exn in
  Bc_compiler.compile ast |> Bc_eval.eval

let%expect_test "run-script" =
  Caml.Sys.readdir script_dir
  |> Array.to_list
  |> List.sort ~compare:String.compare
  |> List.iter ~f:(fun filename ->
         let filename = Caml.Filename.concat script_dir filename in
         Stdio.printf ">> running %s\n%!" filename;
         (try run_one filename with
         | exn -> Stdio.printf "raised: %s\n%!" (Exn.to_string exn));
         Stdio.printf "EOF\n\n%!");
  [%expect
    {|
    >> running scripts/functions.py
    1 -1 1 -1 -1 1 42
    6 0
    6 1
    6 2
    42 42 42
    42 barfoo foobar
    ('foo', [])
    ('foo', ['b', 'c'])
    ('bar', ['b', 'c'])
    EOF |}]
