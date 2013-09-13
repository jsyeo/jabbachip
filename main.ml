open Parser
open Lexer
open Stringofs

let parse =
  let channelf = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel channelf in
  try
    let ast = Parser.prog (Lexer.token) lexbuf in
    close_in channelf;
    print_endline (string_of_jlite_program ~prog:ast)
  with
    End_of_file -> exit 0
