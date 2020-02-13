{
open! Lexing
open! Parser
module String = Base.String
module Stack = Base.Stack
module Queue = Base.Queue
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }

module Env = struct
  type t =
    { indents: int Stack.t
    ; mutable nestings : int
    ; mutable tokens : Parser.token Queue.t
    }

  let create () =
    { indents = Stack.create ()
    ; nestings = 0
    ; tokens = Queue.create ()
    }

  let enter t =
    t.nestings <- t.nestings + 1

  let exit t =
    t.nestings <- t.nestings - 1;
    if t.nestings < 0
    then raise (SyntaxError "too many closing delimiters")

  let last_indent t =
    Option.value (Stack.top t.indents) ~default:0

  let push_indent t indent =
    Stack.push t.indents indent

  let drop_indent t ~until =
    let rec loop acc =
      match Stack.top t.indents with
      | None -> if until = 0 then Some acc else None
      | Some level when level = until -> Some acc
      | Some level when level < until -> None
      | Some _level (* when level > until *) ->
          ignore (Stack.pop_exn t.indents : int);
          loop (acc + 1)
    in
    loop 0

  let token t = Queue.dequeue t.tokens

  let add_tokens t tokens = Queue.enqueue_all t.tokens tokens
end
}

rule read env = parse
  | ' ' { read env lexbuf }
  | "\\\n" { read env lexbuf }
  | "def" { [DEF] }
  | "if" { [IF] }
  | "elif" { [ELIF] }
  | "else" { [ELSE] }
  | "return" { [RETURN] }
  | "del" { [DELETE] }
  | "while" { [WHILE] }
  | "True" { [BOOL true] }
  | "False" { [BOOL false] }
  | "None" { [NONE] }
  | "and" { [OPAND] }
  | "or" { [OPOR] }
  | "break" { [BREAK] }
  | "continue" { [CONTINUE] }
  | "pass" { [PASS] }
  | "for" { [FOR] }
  | "in" { [IN] }
  | "lambda" { [LAMBDA] }
  | "assert" { [ASSERT] }
  | "class" { [CLASS] }
  | "raise" { [RAISE] }
  | "from" { [FROM] }
  | "try" { [TRY] }
  | "except" { [EXCEPT] }
  | "finally" { [FINALLY] }
  | "not" { [OPNOT] }
  | "is" { [OPIS] }
  (* TODO: other string delimiters... *)
  | '"' { [string_double_quote (Buffer.create 1024) lexbuf] }
  | '\'' { [string_single_quote (Buffer.create 1024) lexbuf] }
  | ['0'-'9']+ as int { [INTEGER int] }
  | '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+ as int { [INTEGER int] }
  | '0' ['b' 'B'] ['0'-'1']+ as int { [INTEGER int] }
  | '0' ['o' 'O'] ['0'-'7']+ as int { [INTEGER int] }
  | ['0'-'9']+ '.' ['0'-'9']* as float { [FLOAT float] }
  | ['0'-'9']* '.' ['0'-'9']+ as float { [FLOAT float] }
  | ['0'-'9']+ '.' ['0'-'9']* ['e' 'E'] ['+' '-']? ['0'-'9']+ as float { [FLOAT float] }
  | ['0'-'9']* '.' ['0'-'9']+ ['e' 'E'] ['+' '-']? ['0'-'9']+ as float { [FLOAT float] }
  | '(' { Env.enter env; [LPAREN] }
  | '{' { Env.enter env; [LBRACE] }
  | '[' { Env.enter env; [LBRACK] }
  | ')' { Env.exit env; [RPAREN] }
  | '}' { Env.exit env; [RBRACE] }
  | ']' { Env.exit env; [RBRACK] }
  | '.' { [DOT] }
  | ',' { [COMMA] }
  | "**" { [OPPOWER] }
  | '+' { [OPADD] }
  | '-' { [OPSUB] }
  | '*' { [OPMUL] }
  | '/' { [OPDIV] }
  | "//" { [OPEDIV] }
  | '%' { [OPMOD] }
  | ':' { [COLON] }
  | ';' { [SEMICOLON] }
  | "<<" { [OPLSHIFT] }
  | ">>" { [OPRSHIFT] }
  | "==" { [OPEQ] }
  | "!=" { [OPNEQ] }
  | '<' { [OPLT] }
  | "<=" { [OPLTEQ] }
  | '>' { [OPGT] }
  | ">=" { [OPGTEQ] }
  | '=' { [EQUAL] }
  | "+=" { [ADDEQ] }
  | "-=" { [SUBEQ] }
  | "*=" { [MULEQ] }
  | "/=" { [DIVEQ] }
  | "//=" { [EDIVEQ] }
  | "%=" { [MODEQ] }
  | '~' { [OPINVERT] }
  | '^' { [OPBXOR] }
  | '&' { [OPBAND] }
  | '|' { [OPBOR] }
  (* TODO: handle tabs *)
  (* This discards lines with only spaces in them. *)
  | ('#' [^'\n']*)? '\n' [' ' '\n']* as str {
    for _i = 1 to String.count str ~f:(Base.Char.(=) '\n') do
      next_line lexbuf
    done;
    let indent = String.length str - String.rindex_exn str '\n' - 1 in
    if env.nestings <> 0
    then read env lexbuf
    else
      let last_indent = Env.last_indent env in
      if last_indent = indent
      then [NEWLINE]
      else if last_indent > indent
      then (
        let dropped =
          match Env.drop_indent env ~until:indent with
          | None -> raise (SyntaxError (Printf.sprintf "Unexpected indentation level %d" indent))
          | Some dropped -> dropped
        in
        NEWLINE :: List.init dropped (fun _ -> DEDENT)
      ) else (
        Env.push_indent env indent;
        [NEWLINE; INDENT]
      )
  }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as id { [IDENTIFIER id] }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof {
    match Env.drop_indent env ~until:0 with
    | None -> [ENDMARKER]
    | Some dropped -> List.init dropped (fun _ -> DEDENT) @ [ENDMARKER]
  }
and string_double_quote buf = parse
 | "\\\"" { Buffer.add_char buf '\"'; string_double_quote buf lexbuf }
 | "\\n" { Buffer.add_char buf '\n'; string_double_quote buf lexbuf }
 | "\\t" { Buffer.add_char buf '\t'; string_double_quote buf lexbuf }
 | '"' { STRING (Buffer.contents buf) }
 | _ as c { Buffer.add_char buf c; string_double_quote buf lexbuf }
and string_single_quote buf = parse
 | "\\\'" { Buffer.add_char buf '\''; string_single_quote buf lexbuf }
 | "\\n" { Buffer.add_char buf '\n'; string_single_quote buf lexbuf }
 | "\\t" { Buffer.add_char buf '\t'; string_single_quote buf lexbuf }
 | '\'' { STRING (Buffer.contents buf) }
 | _ as c { Buffer.add_char buf c; string_single_quote buf lexbuf }

{
  let read env lexbuf =
    match Env.token env with
    | Some token -> token
    | None ->
        match read env lexbuf with
        | [] -> assert false
        | token :: tokens ->
          Env.add_tokens env tokens;
          token
}
