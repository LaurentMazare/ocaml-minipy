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
%token NEWLINES
%token EOF

%left OPSUB
%left OPADD

%type <Mini_ast.t> mod_ stmt_list stmt_list_
%type <Mini_ast.stmt> stmt
%type <Mini_ast.expr> expr
%start mod_
%%

mod_:
  | l=stmt_list EOF { l }
;

stmt_list:
  | NEWLINES l=stmt_list_ { l }

stmt_list_:
  | { [] }
  | s=stmt NEWLINES l=stmt_list_ { s :: l }
;

stmt:
  | t=IDENTIFIER EQUAL v=expr { Assign { targets = [ Name t ]; value = v } }
;

expr:
  | IDENTIFIER { Name $1 }
  | STRING { Str $1 }
  | INTEGER { Num (int_of_string $1) }
  | FLOAT { Float (float_of_string $1) }
  | left=expr OPADD right=expr { BinOp { left; op = Add; right } }
  | left=expr OPSUB right=expr { BinOp { left; op = Sub; right } }
;

