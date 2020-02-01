open! Base
open! Lexing

module Error = struct
  type t =
    { message : string
    ; context : string option
    }
  [@@deriving sexp]
end

let ok_exn = function
  | Ok v -> v
  | Error { Error.message; context = None } -> failwith message
  | Error { message; context = Some context } -> failwith (message ^ "\n" ^ context)

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
  | NEWLINE -> "NEWLINE"
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
  | ENDMARKER -> "ENDMARKER"
  | ELSE -> "ELSE"
  | ELIF -> "ELIF"
  | DOT -> "DOT"
  | DELETE -> "DELETE"
  | DEF -> "DEF"
  | DEDENT -> "DEDENT"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | BREAK -> "BREAK"
  | BOOL b -> Printf.sprintf "BOOL<%b>" b
  | OPAND -> "OPAND"
  | OPOR -> "OPOR"

let tokens ?(filename = "unk") in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let env = Lexer.Env.create () in
  let rec loop acc =
    let token = Lexer.read env lexbuf in
    let acc = token :: acc in
    match token with
    | ENDMARKER -> List.rev acc
    | _ -> loop acc
  in
  loop []

let tokens_file filename = Stdio.In_channel.with_file filename ~f:(tokens ~filename)

let get_line filename lineno =
  try
    Stdio.In_channel.with_file filename ~f:(fun in_channel ->
        let rec loop lineno =
          let line = Stdio.In_channel.input_line in_channel in
          if lineno = 1 then line else loop (lineno - 1)
        in
        loop lineno)
  with
  | _ -> None

let parse parse_fun lexbuf =
  let open MenhirLib.General in
  let module I = Parser.MenhirInterpreter in
  let handle_result result = Ok result in
  let env = Lexer.Env.create () in
  let input = I.lexer_lexbuf_to_supplier (Lexer.read env) lexbuf in
  let handle_error error_state =
    let env =
      match error_state with
      | I.HandlingError env -> env
      | _ -> assert false
    in
    match I.stack env |> Lazy.force with
    | Nil -> assert false
    | Cons (I.Element (state, _, start_pos, end_pos), _) ->
      let message =
        try Parser_messages.message (I.number state) with
        | Caml.Not_found -> "unknown"
      in
      let line_pos, col_pos =
        if start_pos.pos_lnum = end_pos.pos_lnum
        then
          ( Int.to_string start_pos.pos_lnum
          , Printf.sprintf
              "%d-%d"
              (start_pos.pos_cnum - start_pos.pos_bol)
              (end_pos.pos_cnum - end_pos.pos_bol) )
        else
          ( Printf.sprintf "%d-%d" start_pos.pos_lnum end_pos.pos_lnum
          , Int.to_string start_pos.pos_lnum )
      in
      let message =
        Printf.sprintf
          "%s:%s:%s: ParseError %s"
          start_pos.pos_fname
          line_pos
          col_pos
          message
      in
      let context = get_line start_pos.pos_fname end_pos.pos_lnum in
      Error { Error.message; context }
  in
  try I.loop_handle handle_result handle_error input (parse_fun lexbuf.lex_curr_p) with
  | Lexer.SyntaxError msg ->
    let pos = lexbuf.lex_curr_p in
    let position =
      Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
    in
    let message = Printf.sprintf "%s: SyntaxError %s" position msg in
    let context = get_line pos.pos_fname pos.pos_lnum in
    Error { Error.message; context }

let parse ?(filename = "unk") in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse Parser.Incremental.mod_ lexbuf

let parse_file filename = Stdio.In_channel.with_file filename ~f:(parse ~filename)
