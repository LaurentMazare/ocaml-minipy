%{
(* Python grammar specification.
   Adapted from: https://docs.python.org/3/reference/grammar.html
   This tries to follow the original LL(1) grammar so is not very
   idiomatic for menhir.
*)

module List = Base.List
module Option = Base.Option
exception ParseError of string
open Ast
let errorf fmt = Printf.ksprintf (fun s -> raise (ParseError s)) fmt

let combine_if ~test ~body ~elif ~orelse =
  let orelse =
    List.rev elif
    |> List.fold ~init:orelse ~f:(fun orelse (test, body) ->
        [ If { test; body; orelse } ])
  in
  If { test; body; orelse }

let empty_args = { args = []; vararg = None; kwonlyargs = []; kwarg = None }

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
  { a with args = List.rev a.args; kwonlyargs = List.rev a.kwonlyargs }

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
%token OPAND OPOR OPLSHIFT OPRSHIFT OPINVERT OPBXOR OPBAND OPBOR
%token OPADD OPSUB OPMUL OPDIV OPEDIV OPMOD OPPOWER
%token ADDEQ SUBEQ MULEQ DIVEQ EDIVEQ MODEQ
%token OPNEQ OPEQ OPLT OPLTEQ OPGT OPGTEQ OPNOT OPIS
%token DOT COMMA EQUAL
%token DEF RETURN DELETE ASSERT IF ELIF ELSE WHILE FOR IN BREAK CONTINUE PASS
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token LAMBDA CLASS RAISE FROM TRY EXCEPT AS FINALLY
%token INDENT DEDENT
%token NEWLINE
%token ENDMARKER

%type <Ast.t> mod_
%type <Ast.stmt list> newline_or_stmt suite orelse
%type <Ast.stmt list> stmt simple_stmt simple_stmt_or_empty
%type <Ast.stmt> compound_stmt small_stmt flow_stmt
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
  | value=testlist { Expr { value } }
  | e1=testlist EQUAL e2=testlist l=assign_right {
    match List.rev (e1 :: e2 :: l) with
    | value :: targets -> Assign { targets; value }
    | _ -> assert false
  }
  | target=testlist op=augassign value=testlist { AugAssign { target; value; op } }
  | DELETE e=testlist { Delete { targets = [ e ] } }
  | PASS { Pass }
  | s=flow_stmt { s }
  | ASSERT test=test msg=assert_message { Assert { test; msg } }
;

augassign:
  | ADDEQ { Add }
  | SUBEQ { Sub }
  | MULEQ { Mult }
  | DIVEQ { Div }
  | EDIVEQ { FloorDiv }
  | MODEQ { Mod }
;

assign_right:
  | { [] }
  | EQUAL l=separated_list(EQUAL, testlist) { l }
;

flow_stmt:
  | BREAK { Break }
  | CONTINUE { Continue }
  | RETURN { Return { value = None } }
  | RETURN v=testlist { Return { value = Some v } }
  | RAISE exc=test? { Raise { exc; cause = None } }
  | RAISE exc=test? FROM cause=test { Raise { exc; cause = Some cause } }
;

suite:
  | s=simple_stmt { s }
  | NEWLINE INDENT l=stmt+ DEDENT { List.concat l }
;

compound_stmt:
  | IF test=test COLON body=suite elif=elif* orelse=orelse { combine_if ~test ~body ~elif ~orelse }
  | WHILE test=test COLON body=suite orelse=orelse { While { test; body; orelse } }
  | FOR target=exprlist IN iter=testlist COLON body=suite orelse=orelse { For { target; iter; body; orelse } }
  | TRY COLON body=suite finalbody=try_finally { Try { body; handlers = []; orelse = []; finalbody } }
  | TRY COLON body=suite handlers=try_except+ orelse=try_orelse f=try_finally?
    { Try { body; handlers; orelse; finalbody = Option.value f ~default:[] } }
  | DEF name=IDENTIFIER LPAREN args=parameters RPAREN COLON body=suite { FunctionDef { name; args; body } }
  | CLASS name=IDENTIFIER args=class_parameters COLON body=suite { ClassDef { name; args; body } }
;

try_except:
    | EXCEPT COLON body=suite { { type_ = None; name = None; body } }
    | EXCEPT e=expr COLON body=suite { { type_ = Some e; name = None; body } }
    | EXCEPT e=expr AS name=IDENTIFIER COLON body=suite { { type_ = Some e; name = Some name; body } }
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
  | OPPOWER id=IDENTIFIER { `kwarg id }
;

elif:
  | ELIF e=test COLON s=suite { e, s }
;

assert_message:
  | { None }
  | COMMA e=test { Some(e) }
;

testlist:
  | e=test { e }
  | e=test COMMA l=testlist_or_empty { Tuple (Array.of_list (e :: l)) }
;

testlist_or_empty:
  | { [] }
  | e=test { [e] }
  | e=test COMMA l=testlist_or_empty { e :: l }
;

exprlist:
  | e=expr { e }
  | e=expr COMMA l=exprlist_or_empty { Tuple (Array.of_list (e :: l)) }
;

exprlist_or_empty:
  | { [] }
  | e=expr { [e] }
  | e=expr COMMA l=exprlist_or_empty { e :: l }
;

test:
  | e=or_test { e }
  | body=or_test IF test=or_test ELSE orelse=test { IfExp { body; test; orelse } }
  | LAMBDA args=parameters COLON body=test { Lambda { args; body } }
;

or_test:
  | e=separated_nonempty_list(OPOR, and_test) {
    match e with
    | [e] -> e
    | values -> BoolOp { op = Or; values }
}
;

and_test:
  | e=separated_nonempty_list(OPAND, not_test) {
    match e with
    | [e] -> e
    | values -> BoolOp { op = And; values }
}
;


not_test:
  | OPNOT operand=not_test { UnaryOp { op = Not; operand } }
  | e=comparison { e }
;

comparison:
  | e=expr { e }
  | l=expr o=comp_op c=comparison {
    let ops_and_exprs =
      match c with
      | Compare { left; ops_and_exprs } -> (o, left) :: ops_and_exprs
      | e -> [ o, e ]
    in
    Compare { left = l; ops_and_exprs }
  }
;

comp_op:
  | OPEQ { Eq }
  | OPNEQ { NotEq }
  | OPLT { Lt }
  | OPLTEQ { LtE }
  | OPGT { Gt }
  | OPGTEQ { GtE }
  | IN { In }
  | OPNOT IN  { NotIn }
  | OPIS { Is }
  | OPIS OPNOT { IsNot }
;

expr:
  | e=xor_expr { e }
  | left=xor_expr OPBOR right=expr { BinOp { left; op = BitOr; right } }
;

xor_expr:
  | e=and_expr { e }
  | left=and_expr OPBXOR right=xor_expr { BinOp { left; op = BitXor; right } }
;

and_expr:
  | e=shift_expr { e }
  | left=shift_expr OPBAND right=and_expr { BinOp { left; op = BitAnd; right } }
;

shift_expr:
  | e=arith_expr { e }
  | left=arith_expr op=opshift right=shift_expr { BinOp { left; op; right } }
;

opshift:
  | OPLSHIFT { LShift }
  | OPRSHIFT { RShift }
;

arith_expr:
  | e=term { e }
  | left=arith_expr op=oparith right=term { BinOp { left; op; right } }
;

oparith:
  | OPADD { Add }
  | OPSUB { Sub }
;

term:
  | e=factor { e }
  | left=term op=opfactor right=factor { BinOp { left; op; right } }
;

opfactor:
  | OPMUL { Mult }
  | OPDIV { Div }
  | OPEDIV { FloorDiv }
  | OPMOD { Mod }
;

factor:
  | OPADD operand=factor { UnaryOp { op = UAdd; operand } }
  | OPSUB operand=factor { UnaryOp { op = USub; operand } }
  | OPINVERT operand=factor { UnaryOp { op = Invert; operand } }
  | e=power { e }
;

power:
  | e=atom_expr { e }
  | left=atom_expr OPPOWER right=factor { BinOp { left; op = Pow; right } }
;

atom_expr:
  | e=atom { e }
  | func=atom_expr LPAREN args=separated_list(COMMA, argument) RPAREN {
    let args, keywords = merge_args args in
    Call { func; args; keywords } }
  | value=atom_expr DOT attr=IDENTIFIER { Attribute { value; attr } }
  | value=atom_expr LBRACK slice=testlist RBRACK { Subscript { value; slice } }
;

atom:
  | LPAREN e=testlist RPAREN { e }
  | LBRACK l=separated_list(COMMA, expr) RBRACK { List (Array.of_list l) }
  | LBRACK elt=expr FOR target=exprlist IN iter=or_test ifs=ifs f=fors RBRACK
    { ListComp { elt; generators = { target; iter; ifs } :: f } }
  | LBRACE key_values=separated_list(COMMA, key_value) RBRACE { Dict { key_values } }
  | IDENTIFIER { Name $1 }
  | STRING { Str $1 }
  | INTEGER { Num (Z.of_string $1) }
  | FLOAT { Float (float_of_string $1) }
  | BOOL { Bool $1 }
  | NONE { None_ }
;

argument:
  | e=test { `Arg e }
  | id=IDENTIFIER EQUAL e=test { `Keyword (id, e) }
;

key_value:
  | key=test COLON value=test { key, value }
;

fors:
  | { [] }
  | FOR target=exprlist IN iter=or_test ifs=ifs f=fors { { target; iter; ifs } :: f }
;

ifs:
  | { [] }
  | IF e=or_test l=ifs { e :: l }
;

orelse:
  | { [] }
  | ELSE COLON b=suite { b }
;
