open Types

let string_of_jlite_type ty = ""

let string_of_jlite_op op = ""

let string_of_var_id id = ""

let string_of_jlite_expr expr =
  match expr with
  | Types.IntLiteral i -> string_of_int i
  | _ -> failwith "OOPS"

let string_of_jlite_stmt stmt = ""

let string_of_var_decl var_decl = ""

let string_of_arg_decl arg_decl = ""

let string_of_md_decl md_decl = ""

let string_of_class_main cls_main = ""

let string_of_class_decl cls_decl = ""

let string_of_jlite_program prog = ""
