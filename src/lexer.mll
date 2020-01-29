{
open! Lexing
open! Parser
exception SyntaxError of string

module Env = struct
  type t =
    { indents: int Base.Stack.t
    ; mutable nestings : int
    }

  let enter t =
    t.nestings <- t.nestings + 1

  let exit t =
    t.nestings <- t.nestings - 1;
    if t.nestings < 0
    then failwith "too many closing delimiters"

  let last_indent t =
    Option.value (Base.Stack.top t.indents) ~default:0

  let push_indent t indent =
    Base.Stack.push t.indents indent
end
}

rule token env = parse
  | ' ' { token env lexbuf }
  | "def" { [DEF] }
  | "if" { [IF] }
  | "elif" { [ELIF] }
  | "else" { [ELSE] }
  | "return" { [RETURN] }
  (* TODO: other string delimiters... *)
  | '"' { [string_token (Buffer.create 1024) lexbuf] }
  | '-'? ['0'-'9']+ as int { [INTEGER int] }
  | '-'? '0' 'x' ['0'-'9' 'A'-'F']+ as int { [INTEGER int] }
  | '-'? ['0'-'9']+ '.' ['0'-'9']+ as float { [FLOAT float] }
  | '(' { Env.enter env; [LPAREN] }
  | '{' { Env.enter env; [LBRACE] }
  | '[' { Env.enter env; [LBRACK] }
  | ')' { Env.exit env; [RPAREN] }
  | '}' { Env.exit env; [RBRACE] }
  | ']' { Env.exit env; [RBRACK] }
  | '.' { [DOT] }
  | ',' { [COMMA] }
  | '+' { [OPADD] }
  | "==" { [OPEQ] }
  | "!=" { [OPNEQ] }
  | '=' { [EQUAL] }
  (* TODO: handle tabs *)
  | '\n' [' ']* as newline_indents {
     let indent = String.length newline_indents - 1 in
     if env.nestings <> 0
     then [NEWLINE]
     else
       let last_indent = Env.last_indent env in
       if last_indent = indent
       then [NEWLINE]
       else if last_indent > indent
       then (
         [NEWLINE; DEDENT]
       ) else (
         Env.push_indent env indent;
         [NEWLINE; INDENT]
       )
  }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as id { [IDENTIFIER id] }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { [EOF] }
and string_token buf = parse
 | "\\\"" { Buffer.add_char buf '\"'; string_token buf lexbuf }
 | "\\n" { Buffer.add_char buf '\n'; string_token buf lexbuf }
 | "\\t" { Buffer.add_char buf '\t'; string_token buf lexbuf }
 | '"' { STRING (Buffer.contents buf) }
 | _ as c { Buffer.add_char buf c; string_token buf lexbuf }

