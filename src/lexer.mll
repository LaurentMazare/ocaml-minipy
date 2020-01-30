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
    then failwith "too many closing delimiters"

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
  | "def" { [DEF] }
  | "if" { [IF] }
  | "elif" { [ELIF] }
  | "else" { [ELSE] }
  | "return" { [RETURN] }
  | "del" { [DELETE] }
  | "while" { [WHILE] }
  | "True" { [BOOL true] }
  | "False" { [BOOL false] }
  | "and" { [OPAND] }
  | "or" { [OPOR] }
  (* TODO: other string delimiters... *)
  | '"' { [string (Buffer.create 1024) lexbuf] }
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
  | '-' { [OPSUB] }
  | '*' { [OPMUL] }
  | '/' { [OPDIV] }
  | ':' { [COLON] }
  | "==" { [OPEQ] }
  | "!=" { [OPNEQ] }
  | '=' { [EQUAL] }
  (* TODO: handle tabs *)
  (* This discards lines with only spaces in them. *)
  | ('#' [^'\n']*)? '\n' [' ' '\n']* as str {
    for _i = 1 to String.count str ~f:(Base.Char.(=) '\n') do
      next_line lexbuf
    done;
    let indent = String.length str - String.rindex_exn str '\n' - 1 in
    if env.nestings <> 0
    then [NEWLINES]
    else
      let last_indent = Env.last_indent env in
      if last_indent = indent
      then [NEWLINES]
      else if last_indent > indent
      then (
        let dropped =
          match Env.drop_indent env ~until:indent with
          | None -> raise (SyntaxError (Printf.sprintf "Unexpected indentation level %d" indent))
          | Some dropped -> dropped
        in
        NEWLINES :: List.init dropped (fun _ -> DEDENT)
      ) else (
        Env.push_indent env indent;
        [NEWLINES; INDENT]
      )
  }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as id { [IDENTIFIER id] }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof {
    match Env.drop_indent env ~until:0 with
    | None -> [EOF]
    | Some dropped -> List.init dropped (fun _ -> DEDENT) @ [EOF]
  }
and string buf = parse
 | "\\\"" { Buffer.add_char buf '\"'; string buf lexbuf }
 | "\\n" { Buffer.add_char buf '\n'; string buf lexbuf }
 | "\\t" { Buffer.add_char buf '\t'; string buf lexbuf }
 | '"' { STRING (Buffer.contents buf) }
 | _ as c { Buffer.add_char buf c; string buf lexbuf }


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
