%{
(* Python grammar specification.
   Adapted from: https://docs.python.org/3/reference/grammar.html
*)

module List = Base.List
module Option = Base.Option

let combine_if ~test ~body ~elif ~orelse =
  let orelse =
    List.rev elif
    |> List.fold ~init:orelse ~f:(fun orelse (test, body) ->
        [ Ast.If { test; body; orelse } ])
  in
  Ast.If { test; body; orelse }
%}

%token <string> INTEGER
%token <string> FLOAT
%token <string> IDENTIFIER STRING
%token <bool> BOOL
%token COLON SEMICOLON
%token OPAND OPOR
%token OPADD OPSUB OPMUL OPDIV OPEDIV OPMOD
%token ADDEQ SUBEQ MULEQ DIVEQ EDIVEQ MODEQ
%token OPNEQ OPEQ OPLT OPLTEQ OPGT OPGTEQ
%token DOT COMMA EQUAL
%token DEF RETURN DELETE ASSERT IF ELIF ELSE WHILE FOR IN BREAK CONTINUE PASS
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token LAMBDA
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
;

suite:
  | s=simple_stmt { s }
  | NEWLINE INDENT l=stmt+ DEDENT { List.concat l }
;

compound_stmt:
  | IF test=expr COLON body=suite elif=elif* orelse=orelse { combine_if ~test ~body ~elif ~orelse }
  | WHILE test=expr COLON body=suite orelse=orelse { While { test; body; orelse } }
  | FOR target=expr_or_tuple IN iter=expr COLON body=suite orelse=orelse { For { target; iter; body; orelse } }
  | DEF name=IDENTIFIER LPAREN args=separated_list(COMMA, IDENTIFIER) RPAREN COLON body=suite
    { FunctionDef { name; args; body }}
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
  | func=expr LPAREN args=separated_list(COMMA, expr) RPAREN { Call { func; args } }
  | value=expr DOT attr=IDENTIFIER { Attribute { value; attr } }
  | LPAREN e=expr_or_tuple RPAREN { e }
  | LBRACK l=separated_list(COMMA, expr) RBRACK { List (Array.of_list l) }
  | LBRACE key_values=separated_list(COMMA, key_value) RBRACE { Dict { key_values } }
  | value=expr LBRACK slice=expr_or_tuple RBRACK { Subscript { value; slice } }
  | LAMBDA args=separated_list(COMMA, IDENTIFIER) COLON body=expr { Lambda { args; body } }
;

key_value:
  | key=expr COLON value=expr { key, value }
;

orelse:
  | { [] }
  | ELSE COLON b=suite { b }
;
