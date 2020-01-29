%{
%}

%token <string> INTEGER
%token <string> FLOAT
%token <string> IDENTIFIER STRING
%token OPADD OPSUB OPMUL OPDIV OPEDIV OPMOD
%token OPNEQ OPEQ
%token DOT COMMA EQUAL
%token DEF RETURN IF ELIF ELSE WHILE FOR BREAK
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token INDENT DEDENT
%token NEWLINE
%token EOF

%type <Mini_ast.t> stmt_list mod_
%type <Mini_ast.stmt> stmt
%type <Mini_ast.expr> expr
%start mod_
%%

mod_:
  | EOF { [] }
  | v=stmt_list EOF { v }
;

stmt_list:
  | NEWLINE { [] }
  | NEWLINE tl=stmt_list { tl }
  | hd=stmt NEWLINE tl=stmt_list { hd::tl }
;

stmt:
  | t=IDENTIFIER EQUAL v=expr { Assign { targets = [ Name t ]; value = v } }
;

expr:
  | IDENTIFIER { Name $1 }
  | STRING { Str $1 }
  | INTEGER { Num (int_of_string $1) }
  | FLOAT { Float (float_of_string $1) }
;

