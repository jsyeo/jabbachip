%{
  open Printf
  open Types
  module AST = Types
%}

%token CLASS IF ELSE WHILE RETURN THIS NEW NULL PRINTLN READLN

%token INT BOOL STRING VOID

%token <int> INT
%token <string> STRING
%token <string> ID CNAME
%token LEFT_BRACE RIGHT_BRACE LEFT_PAREN RIGHT_PAREN LEFT_BRACK RIGHT_BRACK
%token SEMICOLON COMMA DOT

%token TRUE FALSE

%token PLUS MINUS MULTIPLY DIVIDE
%token LT GT LTEQ GTEQ EQ NOTEQ
%token AND OR
%token NEGATE BOOL_NEGATE
%token ASSIGN
%token EOF

/* lowest precedence */
%left OR
%left AND
%left EQ NOTEQ
%left LT GT LTEQ GTEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE
%right NEGATE BOOL_NEGATE
/* highest precedence */

%start prog
%type <Types.jlite_stmt list> prog
%%

prog:
 | stmtlist EOF   { $1 }

stmtlist:
 | { [] }
 | stmt stmtlist { $1 :: $2 }

nonempty_stmtlist:
 | stmt                   { [$1] }
 | stmt nonempty_stmtlist { $1 :: $2 }

block:
 | LEFT_BRACE stmtlist RIGHT_BRACE { $2 }

nonempty_block:
 | LEFT_BRACE nonempty_stmtlist RIGHT_BRACE { $2 }

stmt:
 | ID ASSIGN expr SEMICOLON                         { AssignStmt(SimpleVarId($1), $3) }
 | IF LEFT_PAREN expr RIGHT_PAREN nonempty_block ELSE nonempty_block { IfStmt($3, $5, $7) }
 | WHILE LEFT_PAREN expr RIGHT_PAREN block          { WhileStmt($3, $5) }
 | PRINTLN LEFT_PAREN expr RIGHT_PAREN SEMICOLON    { PrintStmt($3) }
 | field ASSIGN expr SEMICOLON                      { AssignFieldStmt($1, $3) }
 | mdcall SEMICOLON                                 { MdCallStmt($1) }

field:
 | atom DOT ID                 { FieldAccess($1, SimpleVarId($3)) }

mdcall:
 | atom LEFT_PAREN exprlist RIGHT_PAREN { MdCall($1, $3) }

atom:
 | field                       { $1 }
 | mdcall                      { $1 }
 | THIS                        { ThisWord }
 | NEW CNAME                   { ObjectCreate($2) }
 | ID                          { Var(SimpleVarId($1)) }
 | LEFT_PAREN expr RIGHT_PAREN { $2 }
 | NULL                        { NullWord }

exprlist:
 | { [] }
 | expr                { [$1] }
 | expr COMMA exprlist { $1 :: $3 }

expr:
 | LEFT_PAREN expr RIGHT_PAREN    { $2 }
 /* arithmetic operators */
 | expr PLUS expr                 { BinaryExpr(ArithmeticOp("+"), $1, $3) }
 | expr MINUS expr                { BinaryExpr(ArithmeticOp("-"), $1, $3) }
 | expr MULTIPLY expr             { BinaryExpr(ArithmeticOp("*"), $1, $3) }
 | expr DIVIDE expr               { BinaryExpr(ArithmeticOp("/"), $1, $3) }
 | MINUS expr %prec NEGATE        { UnaryExpr(ArithmeticOp("-"), $2) }
 /* relational operators */
 | expr LT expr                   { BinaryExpr(RelationalOp("<"), $1, $3) }
 | expr GT expr                   { BinaryExpr(RelationalOp(">"), $1, $3) }
 | expr LTEQ expr                 { BinaryExpr(RelationalOp("<="), $1, $3) }
 | expr GTEQ expr                 { BinaryExpr(RelationalOp(">="), $1, $3) }
 | expr EQ expr                   { BinaryExpr(RelationalOp("=="), $1, $3) }
 | expr NOTEQ expr                { BinaryExpr(RelationalOp("!="), $1, $3) }
 /* boolean operators */
 | expr AND expr                  { BinaryExpr(BooleanOp("&&"), $1, $3) }
 | expr OR expr                   { BinaryExpr(BooleanOp("||"), $1, $3) }
 | BOOL_NEGATE expr               { UnaryExpr(BooleanOp("!"), $2) }
 | TRUE                           { BoolLiteral true }
 | FALSE                          { BoolLiteral false }
 | ID                             { Var(SimpleVarId($1)) }
 | INT                            { IntLiteral $1 }
 | STRING                         { StringLiteral $1 }

%%
