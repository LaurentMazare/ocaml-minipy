%{
(* Python grammar specification.
   Adapted from: https://docs.python.org/3/reference/grammar.html
*)

module List = Base.List
module Option = Base.Option
exception ParseError of string
let errorf fmt = Printf.ksprintf (fun s -> raise (ParseError s)) fmt

let combine_if ~test ~body ~elif ~orelse =
  let orelse =
    List.rev elif
    |> List.fold ~init:orelse ~f:(fun orelse (test, body) ->
        [ Ast.If { test; body; orelse } ])
  in
  Ast.If { test; body; orelse }

let empty_args = { Ast.args = []; vararg = None; kwonlyargs = []; kwarg = None }

let merge_parameters parameters =
  let a =
    List.fold parameters
      ~init:empty_args
      ~f:(fun acc arg ->
        match arg with
        | `arg id ->
            if Option.is_some acc.vararg
            then errorf "positional argument %s after vararg" id;
            if Option.is_some acc.kwarg
            then errorf "positional argument %s after kwarg" id;
            { acc with args = id :: acc.args }
        | `kwonlyarg (id, e) ->
            if Option.is_some acc.kwarg
            then errorf "keyword argument %s after kwarg" id;
            { acc with kwonlyargs  = (id, e) :: acc.kwonlyargs }
        | `vararg id ->
            if Option.is_some acc.vararg
            then errorf "duplicate vararg" id;
            if Option.is_some acc.kwarg
            then errorf "vararg %s after kwarg" id;
            { acc with vararg = Some id }
        | `kwarg id ->
            if Option.is_some acc.kwarg
            then errorf "duplicate kwarg" id;
            { acc with kwarg = Some id }
      )
  in
  let all_ids =
    a.args @ Option.to_list a.vararg @ Option.to_list a.kwarg @ List.map a.kwonlyargs ~f:fst
  in
  Option.iter (List.find_a_dup all_ids ~compare:String.compare) ~f:(fun dup_id ->
    errorf "duplicate argument name %s" dup_id);
  { a with Ast.args = List.rev a.args; kwonlyargs = List.rev a.kwonlyargs }

let merge_args args =
  let args, keywords =
    List.fold args ~init:([], [])  ~f:(fun (acc_a, acc_kw) arg ->
      match arg with
      | `Arg arg ->
          if not (List.is_empty acc_kw)
          then errorf "positional argument follows keyword argument";
          arg :: acc_a, acc_kw
      | `Keyword kwarg -> acc_a, kwarg :: acc_kw)
  in
  List.rev args, List.rev keywords
%}

%token <string> INTEGER
%token <string> FLOAT
%token <string> IDENTIFIER STRING
%token <bool> BOOL
%token NONE
%token COLON SEMICOLON
%token OPAND OPOR
%token OPADD OPSUB OPMUL OPDIV OPEDIV OPMOD
%token ADDEQ SUBEQ MULEQ DIVEQ EDIVEQ MODEQ
%token OPNEQ OPEQ OPLT OPLTEQ OPGT OPGTEQ
%token DOT COMMA EQUAL
%token DEF RETURN DELETE ASSERT IF ELIF ELSE WHILE FOR IN BREAK CONTINUE PASS
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token LAMBDA CLASS RAISE FROM TRY EXCEPT AS FINALLY
%token INDENT DEDENT
%token NEWLINE
%token ENDMARKER

%nonassoc COLON
%left IF ELSE
%left OPOR
%left OPAND
%left OPNEQ
%left OPEQ
%left OPLT
%left OPLTEQ
%left OPGT
%left OPGTEQ
%left OPADD
%left OPSUB
%left OPMUL
%left OPDIV
%left OPEDIV
%left OPMOD
%left DOT
%nonassoc LPAREN
%nonassoc LBRACK

%type <Ast.t> mod_
%type <Ast.stmt list> newline_or_stmt suite orelse
%type <Ast.stmt list> stmt simple_stmt simple_stmt_or_empty
%type <Ast.stmt> compound_stmt small_stmt flow_stmt
%type <Ast.expr> expr expr_or_tuple
%type <Ast.expr list> expr_or_tuple_or_empty
%type <Ast.expr * Ast.stmt list> elif
%type <Ast.expr option> assert_message
%start mod_
%%

mod_:
  | l=newline_or_stmt* ENDMARKER { List.concat l }
;

newline_or_stmt:
  | NEWLINE { [] }
  | s=stmt { s }
;

stmt:
  | s=simple_stmt { s }
  | s=compound_stmt { [ s ] }
;

simple_stmt:
  | s=small_stmt NEWLINE { [ s ] }
  | s=small_stmt SEMICOLON l=simple_stmt_or_empty { s :: l }
;

simple_stmt_or_empty:
  | NEWLINE { [] }
  | s=small_stmt NEWLINE { [s] }
  | s=small_stmt SEMICOLON l=simple_stmt_or_empty { s :: l }
;

small_stmt:
  | value=expr_or_tuple { Expr { value } }
  | e1=expr_or_tuple EQUAL e2=expr_or_tuple l=assign_right {
    match List.rev (e1 :: e2 :: l) with
    | value :: targets -> Assign { targets; value }
    | _ -> assert false
  }
  | target=expr_or_tuple ADDEQ value=expr_or_tuple { AugAssign { target; value; op = Add } }
  | target=expr_or_tuple SUBEQ value=expr_or_tuple { AugAssign { target; value; op = Sub } }
  | target=expr_or_tuple MULEQ value=expr_or_tuple { AugAssign { target; value; op = Mult } }
  | target=expr_or_tuple DIVEQ value=expr_or_tuple { AugAssign { target; value; op = Div } }
  | target=expr_or_tuple EDIVEQ value=expr_or_tuple { AugAssign { target; value; op = FloorDiv } }
  | target=expr_or_tuple MODEQ value=expr_or_tuple { AugAssign { target; value; op = Mod } }
  | DELETE e=expr_or_tuple { Delete { targets = [ e ] } }
  | PASS { Pass }
  | s=flow_stmt { s }
  | ASSERT test=expr msg=assert_message { Assert { test; msg } }
;

assign_right:
  | { [] }
  | EQUAL l=separated_list(EQUAL, expr_or_tuple) { l }
;

flow_stmt:
  | BREAK { Break }
  | CONTINUE { Continue }
  | RETURN { Return { value = None } }
  | RETURN v=expr_or_tuple { Return { value = Some v } }
  | RAISE exc=expr? { Raise { exc; cause = None } }
  | RAISE exc=expr? FROM cause=expr { Raise { exc; cause = Some cause } }
;

suite:
  | s=simple_stmt { s }
  | NEWLINE INDENT l=stmt+ DEDENT { List.concat l }
;

compound_stmt:
  | IF test=expr COLON body=suite elif=elif* orelse=orelse { combine_if ~test ~body ~elif ~orelse }
  | WHILE test=expr COLON body=suite orelse=orelse { While { test; body; orelse } }
  | FOR target=expr_or_tuple IN iter=expr COLON body=suite orelse=orelse { For { target; iter; body; orelse } }
  | TRY COLON body=suite finalbody=try_finally { Try { body; handlers = []; orelse = []; finalbody } }
  | TRY COLON body=suite handlers=try_except+ orelse=try_orelse f=try_finally?
    { Try { body; handlers; orelse; finalbody = Option.value f ~default:[] } }
  | DEF name=IDENTIFIER LPAREN args=parameters RPAREN COLON body=suite { FunctionDef { name; args; body } }
  | CLASS name=IDENTIFIER args=class_parameters COLON body=suite { ClassDef { name; args; body } }
;

try_except:
    | EXCEPT COLON body=suite { { Ast.type_ = None; name = None; body } }
    | EXCEPT e=expr COLON body=suite { { Ast.type_ = Some e; name = None; body } }
    | EXCEPT e=expr AS name=IDENTIFIER COLON body=suite { { Ast.type_ = Some e; name = Some name; body } }
;

try_orelse:
    | { [] }
    | ELSE COLON f=suite { f }
;

try_finally:
    | FINALLY COLON f=suite { f }
;

class_parameters:
    | { empty_args }
    | LPAREN args=parameters RPAREN { args }
;

parameters:
  | l=separated_list(COMMA, parameter) { merge_parameters l }
;

parameter:
  | id=IDENTIFIER { `arg id }
  | id=IDENTIFIER EQUAL e=expr { `kwonlyarg (id, e) }
  | OPMUL id=IDENTIFIER { `vararg id }
  | OPMUL OPMUL id=IDENTIFIER { `kwarg id }
;

elif:
  | ELIF e=expr COLON s=suite { e, s }
;

assert_message:
  | { None }
  | COMMA e=expr { Some(e) }
;

expr_or_tuple:
  | e=expr { e }
  | e=expr COMMA l=expr_or_tuple_or_empty { Tuple (Array.of_list (e :: l)) }
;

expr_or_tuple_or_empty:
  | { [] }
  | e=expr { [e] }
  | e=expr COMMA l=expr_or_tuple_or_empty { e :: l }
;

expr:
  | IDENTIFIER { Name $1 }
  | STRING { Str $1 }
  | INTEGER { Num (int_of_string $1) }
  | FLOAT { Float (float_of_string $1) }
  | BOOL { Bool $1 }
  | NONE { None_ }
  | left=expr OPAND right=expr { BoolOp { values = [left; right]; op = And } }
  | left=expr OPOR right=expr { BoolOp { values = [left; right]; op = Or } }
  | left=expr OPEQ right=expr { Compare { left; ops = Eq; comparators = right } }
  | left=expr OPNEQ right=expr { Compare { left; ops = NotEq; comparators = right } }
  | left=expr OPLT right=expr { Compare { left; ops = Lt; comparators = right } }
  | left=expr OPLTEQ right=expr { Compare { left; ops = LtE; comparators = right } }
  | left=expr OPGT right=expr { Compare { left; ops = Gt; comparators = right } }
  | left=expr OPGTEQ right=expr { Compare { left; ops = GtE; comparators = right } }
  | left=expr OPMUL right=expr { BinOp { left; op = Mult; right } }
  | left=expr OPDIV right=expr { BinOp { left; op = Div; right } }
  | left=expr OPEDIV right=expr { BinOp { left; op = FloorDiv; right } }
  | left=expr OPMOD right=expr { BinOp { left; op = Mod; right } }
  | left=expr OPADD right=expr { BinOp { left; op = Add; right } }
  | left=expr OPSUB right=expr { BinOp { left; op = Sub; right } }
  | OPSUB operand=expr { UnaryOp { op = USub; operand } }
  | OPADD operand=expr { UnaryOp { op = UAdd; operand } }
  | body=expr IF test=expr ELSE orelse=expr { IfExp { body; test; orelse } }
  | func=expr LPAREN args=separated_list(COMMA, argument) RPAREN {
    let args, keywords = merge_args args in
    Call { func; args; keywords } }
  | value=expr DOT attr=IDENTIFIER { Attribute { value; attr } }
  | LPAREN e=expr_or_tuple RPAREN { e }
  | LBRACK l=separated_list(COMMA, expr) RBRACK { List (Array.of_list l) }
  | LBRACK elt=expr FOR target=expr_or_tuple IN iter=expr ifs=ifs f=fors RBRACK
    { ListComp { elt; generators = { target; iter; ifs } :: f } }
  | LBRACE key_values=separated_list(COMMA, key_value) RBRACE { Dict { key_values } }
  | value=expr LBRACK slice=expr_or_tuple RBRACK { Subscript { value; slice } }
  | LAMBDA args=parameters COLON body=expr { Lambda { args; body } }
;

argument:
  | e=expr { `Arg e }
  | id=IDENTIFIER EQUAL e=expr { `Keyword (id, e) }
;

key_value:
  | key=expr COLON value=expr { key, value }
;

fors:
  | { [] }
  | FOR target=expr_or_tuple IN iter=expr ifs=ifs f=fors { { Ast.target; iter; ifs } :: f }
;

ifs:
  | { [] }
  | IF e=expr l=ifs { e :: l }
;

orelse:
  | { [] }
  | ELSE COLON b=suite { b }
;
