%{
  Open Printf
  Module J = open Types
%}

%token CLASS IF ELSE WHILE RETURN THIS NEW

%token <int> INT
%token <string> ID CNAME
%token LEFT_BRACE RIGHT_BRACE LEFT_PAREN RIGHT_PAREN
%token SEMICOLON COMMA

%token TRUE FALSE

%token LT GT LTEQ GTEQ EQ NOTEQ
%token PLUS MINUS MULTIPLY DIVIDE
%token ASSSIGN
%token EOF

%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG

%start prog
%type <J.jlite_expr> prog
%%

prog:
 | EOF    { }
 | expr    { }

expr:
 | INT    { J.IntLiteral($1) }
%%
