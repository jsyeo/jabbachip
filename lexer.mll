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
  create_hashtable 8 [
    ("class", CLASS);
    ("if", IF);
    ("else", ELSE);
    ("while", WHILE);
    ("return", RETURN);
    ("this", THIS);
    ("new", NEW)
  ]
}

let int = ['1'-'9']['0'-'9']*
let cname = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let id = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token =
  parse
  | white         { token lexbuf}
  | newline       { next_line lexbuf; token lexbuf}
  | int           { INT (int_of_string (Lexing.lexeme lexbuf))}
  | cname as word { CNAME word }
  | id as word
      { try
          let token = Hashtbl.find keyword_table word in
          printf "keyword: %s\n" word;
          token
        with Not_found ->
          printf "identifier: %s\n" word;
          ID word
      }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | '"'           { read_string (Buffer.create 17) lexbuf}
  | "<"           { LT }
  | ">"           { GT }
  | "<="          { LTEQ }
  | ">="          { GTEQ }
  | "=="          { EQ }
  | "!="          { NOTEQ }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { MULTIPLY }
  | '/'           { DIVIDE }
  | '='           { ASSSIGN }
  | '{'           { LEFT_BRACE }
  | '}'           { RIGHT_BRACE }
  | '('           { LEFT_PAREN }
  | ')'           { RIGHT_PAREN }
  | '['           { RIGHT_BRACK }
  | ']'           { LEFT_BRACK }
  | ';'           { SEMICOLON }
  | ','           { COMMA }
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
