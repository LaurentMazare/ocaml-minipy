open! Base
open! Lexing

let verbose = true

let token_to_string (token : Parser.token) =
  match token with
  | WHILE -> "WHILE"
  | STRING s -> Printf.sprintf "STRING<%s>" s
  | RPAREN -> "RPAREN"
  | RETURN -> "RETURN"
  | RBRACK -> "RBRACK"
  | RBRACE -> "RBRACE"
  | OPSUB -> "OPSUB"
  | OPNEQ -> "OPNEQ"
  | OPMUL -> "OPMUL"
  | OPMOD -> "OPMOD"
  | OPEQ -> "OPEQ"
  | OPEDIV -> "OPEDIV"
  | OPDIV -> "OPDIV"
  | OPADD -> "OPADD"
  | NEWLINES -> "NEWLINES"
  | LPAREN -> "LPAREN"
  | LBRACK -> "LBRACK"
  | LBRACE -> "LBRACE"
  | INTEGER s -> Printf.sprintf "INTEGER<%s>" s
  | INDENT -> "INDENT"
  | IF -> "IF"
  | IDENTIFIER s -> Printf.sprintf "IDENTIFIER<%s>" s
  | FOR -> "FOR"
  | FLOAT s -> Printf.sprintf "FLOAT<%s>" s
  | EQUAL -> "EQUAL"
  | EOF -> "EOF"
  | ELSE -> "ELSE"
  | ELIF -> "ELIF"
  | DOT -> "DOT"
  | DELETE -> "DELETE"
  | DEF -> "DEF"
  | DEDENT -> "DEDENT"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | BREAK -> "BREAK"

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse ~filename =
  Stdio.In_channel.with_file filename ~f:(fun in_channel ->
      let lexbuf = Lexing.from_channel in_channel in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let env = Lexer.Env.create () in
      let read lexbuf =
        let token = Lexer.read env lexbuf in
        if verbose then Stdio.printf "token: <%s>\n%!" (token_to_string token);
        token
      in
      try Ok (Parser.mod_ read lexbuf) with
      | Lexer.SyntaxError msg ->
        Or_error.errorf "%s: syntax error %s\n" (print_position lexbuf) msg
      | Parser.Error -> Or_error.errorf "%s: parsing error\n" (print_position lexbuf))
