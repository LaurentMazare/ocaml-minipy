%{
(* Python grammar specification.
   Adapted from: https://docs.python.org/3/reference/grammar.html
*)

module List = Base.List

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
%token OPNEQ OPEQ
%token DOT COMMA EQUAL
%token DEF RETURN DELETE IF ELIF ELSE WHILE FOR BREAK
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token INDENT DEDENT
%token NEWLINE
%token ENDMARKER

%left IF ELSE
%left OPOR
%left OPAND
%left OPNEQ
%left OPEQ
%left OPADD
%left OPSUB
%left OPMUL
%left OPDIV
%left DOT
%nonassoc LPAREN

%type <Ast.t> mod_
%type <Ast.stmt list> newline_or_stmt suite orelse
%type <Ast.stmt list> stmt simple_stmt semicolon_simple_stmt simple_stmt_or_empty
%type <Ast.stmt> compound_stmt small_stmt flow_stmt
%type <Ast.expr> expr
%type <Ast.expr * Ast.stmt list> elif
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
  | s=small_stmt l=semicolon_simple_stmt { s :: l }
;

semicolon_simple_stmt:
  | NEWLINE { [] }
  | SEMICOLON l=simple_stmt_or_empty { l }
;
 
simple_stmt_or_empty:
  | NEWLINE { [] }
  | s=small_stmt l=semicolon_simple_stmt { s :: l }
;

small_stmt:
  | value=expr { Expr { value } }
  | target=expr EQUAL value=expr { Assign { targets = [ target ]; value } }
  | DELETE e=expr { Delete { targets = [ e ] } }
  | s=flow_stmt { s }
;

flow_stmt:
  | RETURN { Return { value = None } }
  | RETURN v=expr { Return { value = Some v } }
;

suite:
  | s=simple_stmt { s }
  | NEWLINE INDENT l=stmt+ DEDENT { List.concat l }
;

compound_stmt:
  | IF test=expr COLON body=suite elif=elif* orelse=orelse { combine_if ~test ~body ~elif ~orelse }
  | WHILE test=expr COLON body=suite orelse=orelse { While { test; body; orelse } }
  | DEF name=IDENTIFIER LPAREN args=separated_list(COMMA, IDENTIFIER) RPAREN COLON body=suite
    { FunctionDef { name; args; body }}
;

elif:
  | ELIF e=expr COLON s=suite { e, s }
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
  | left=expr OPMUL right=expr { BinOp { left; op = Mult; right } }
  | left=expr OPDIV right=expr { BinOp { left; op = Div; right } }
  | left=expr OPADD right=expr { BinOp { left; op = Add; right } }
  | left=expr OPSUB right=expr { BinOp { left; op = Sub; right } }
  | body=expr IF test=expr ELSE orelse=expr { IfExp { body; test; orelse } }
  | func=expr LPAREN args=separated_list(COMMA, expr) RPAREN { Call { func; args } }
  | value=expr DOT attr=IDENTIFIER { Attribute { value; attr } }
  | LPAREN e=expr RPAREN { e }
  | LBRACK l=separated_list(COMMA, expr) RBRACK { List (Array.of_list l) }
;

orelse:
  | { [] }
  | ELSE COLON b=suite { b }
;
