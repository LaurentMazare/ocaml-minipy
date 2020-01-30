open! Base
open! Lexing

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse ~filename =
  Stdio.In_channel.with_file filename ~f:(fun in_channel ->
      let lexbuf = Lexing.from_channel in_channel in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let env = Lexer.Env.create () in
      let read = Lexer.read env in
      try Ok (Parser.mod_ read lexbuf) with
      | Lexer.SyntaxError msg ->
        Or_error.errorf "%s: syntax error %s\n" (print_position lexbuf) msg
      | Parser.Error -> Or_error.errorf "%s: parsing error\n" (print_position lexbuf))
