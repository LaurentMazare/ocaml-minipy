open! Base
open Stdio
module I = Minipy.Interpreter
module Parse = Minipy.Parse

let () =
  let env = I.Env.empty ~builtins:I.default_builtins in
  let protect ~f =
    try f () with
    | I.RuntimeError message -> printf "RuntimeError: %s\n%!" message
  in
  let eval_stmts stmts = protect ~f:(fun () -> I.eval_stmts env stmts) in
  let rec repl () =
    Stdio.printf ">>> %!";
    match In_channel.input_line stdin with
    | None -> ()
    | Some line ->
      let stmts = Parse.parse_string (line ^ "\n") in
      (match stmts with
      | Error { message; context } ->
        printf "ParseError: %s\n%!" message;
        Option.iter context ~f:(fun c -> printf "%s\n%!" c)
      | Ok stmts ->
        (match List.last stmts with
        | None -> ()
        | Some (Expr { value }) ->
          let stmts = List.drop_last_exn stmts in
          eval_stmts stmts;
          protect ~f:(fun () ->
              match I.eval_expr env value with
              | Val_none -> ()
              | v -> I.Value.to_string v ~escape_special_chars:false |> print_endline)
        | Some _ -> eval_stmts stmts));
      repl ()
  in
  repl ()
