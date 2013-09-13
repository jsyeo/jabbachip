{
open Parser
open Lexing
open Printf

exception SyntaxError of string

let next_line lexbuf = 
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1}

let create_hashtable size initial_list =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) initial_list;
  tbl

let keyword_table =
  create_hashtable 11 [
    ("class", CLASS);
    ("if", IF);
    ("else", ELSE);
    ("while", WHILE);
    ("return", RETURN);
    ("this", THIS);
    ("new", NEW);
    ("null", NULL);
    ("println", PRINTLN);
    ("readln", READLN);
    ("main", MAIN)
  ]

let reserved_cname_table =
  create_hashtable 4 [
    ("Int", INT_TYPE);
    ("Bool", BOOL_TYPE);
    ("String", STRING_TYPE);
    ("Void", VOID_TYPE)
  ]
}

let int = ['0'-'9']
let cname = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let id = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token =
  parse
  | white         { token lexbuf}
  | newline       { next_line lexbuf; token lexbuf}
  | int+          { INT (int_of_string (Lexing.lexeme lexbuf))}
  | cname as word
      { try
          let token = Hashtbl.find reserved_cname_table word in
          printf "reserved cname: %s\n" word;
          token
        with Not_found ->
          printf "cname: %s\n" word;
          CNAME word }
  | "false"       { FALSE }
  | "true"        { TRUE }
  | id as word
      { try
          let token = Hashtbl.find keyword_table word in
          printf "keyword: %s\n" word;
          token
        with Not_found ->
          printf "identifier: %s\n" word;
          ID word
      }
  | '"'           { read_string (Buffer.create 17) lexbuf}
  | "<"           { LT }
  | ">"           { GT }
  | "<="          { LTEQ }
  | ">="          { GTEQ }
  | "=="          { EQ }
  | "!="          { NOTEQ }
  | "!"           { BOOL_NEGATE }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { MULTIPLY }
  | '/'           { DIVIDE }
  | '='           { ASSIGN }
  | '{'           { LEFT_BRACE }
  | '}'           { RIGHT_BRACE }
  | '('           { LEFT_PAREN }
  | ')'           { RIGHT_PAREN }
  | '['           { RIGHT_BRACK }
  | ']'           { LEFT_BRACK }
  | ';'           { SEMICOLON }
  | ','           { COMMA }
  | '.'           { DOT }
  | "&&"          { AND }
  | "||"          { OR }
  | _             { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }

and read_string buf =
  parse
  | '"'         { STRING (Buffer.contents buf) }
  | '\\' '/'    {Buffer.add_char buf '/'; read_string buf lexbuf}
  | '\\' '\\'   {Buffer.add_char buf 

'\\'; read_string buf lexbuf}
  | '\\' 'b'    {Buffer.add_char buf '\b'; read_string buf lexbuf}
  | '\\' 'f'    {Buffer.add_char buf '\012'; read_string buf lexbuf}
  | '\\' 'n'    {Buffer.add_char buf '\n'; read_string buf lexbuf}
  | '\\' 'r'    {Buffer.add_char buf '\r'; read_string buf lexbuf}
  | '\\' 't'    {Buffer.add_char buf '\t'; read_string buf lexbuf}
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
