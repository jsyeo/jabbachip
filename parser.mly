%{
  open Printf
  open Types
  module AST = Types
%}

%token CLASS IF ELSE WHILE RETURN THIS NEW

%token <int> INT
%token <string> STRING
%token <string> ID CNAME
%token LEFT_BRACE RIGHT_BRACE LEFT_PAREN RIGHT_PAREN LEFT_BRACK RIGHT_BRACK
%token SEMICOLON COMMA

%token TRUE FALSE

%token LT GT LTEQ GTEQ EQ NOTEQ
%token PLUS MINUS MULTIPLY DIVIDE
%token AND OR
%token ASSSIGN
%token EOF

%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG

%start prog
%type <Types.jlite_expr> prog
%%

prog:
 | exp EOF   { Types.IntLiteral($1) }


exp:
 | INT    { $1 }
%%
