open Types

let rec string_of_list pr lst =
  "[" ^
    begin
      match lst with
      | [] -> "]"
      | [x] -> pr x ^ "]"
      | (x::xs) -> pr x ^ "; " ^ string_of_list pr xs
    end

let string_of_jlite_type ty =
  match ty with
  | IntT -> "IntT"
  | BoolT -> "BoolT"
  | StringT -> "StringT"
  | ObjectT(cls_name) -> Printf.sprintf "ObjectT(%s)" cls_name
  | VoidT -> "VoidT"
  | Unknown -> "Unknown"

let string_of_jlite_op op =
  match op with
  | BooleanOp s -> s
  | RelationalOp s -> s
  | ArithmeticOp s -> s
  | UnaryOp s -> s

let string_of_typed_var_id (s, ty, i) =
  Printf.sprintf "(%s, %s, %s)"
    s (string_of_jlite_type ty) (string_of_int i)

let string_of_var_id id =
  match id with
  | SimpleVarId s -> "SimpleVarId(" ^ s ^ ")"
  | TypedVarId s -> Printf.sprintf "TypedVarId(%s)" (string_of_typed_var_id s)

let rec string_of_jlite_expr expr =
  match expr with
  | IntLiteral i -> "IntLiteral(" ^ string_of_int i ^ ")"
  | BoolLiteral b -> "BoolLiteral(" ^ string_of_bool b ^ ")"
  | StringLiteral s -> "StringLiteral(\"" ^ s ^ "\")"
  | Var var_id -> Printf.sprintf "Var(%s)" (string_of_var_id var_id)
  | UnaryExpr(op, expr) -> "UnaryExpr(" ^ string_of_jlite_op op ^ ", " ^ string_of_jlite_expr expr ^ ")"
  | BinaryExpr(op, expr_left, expr_right) ->
    "Binary(" ^string_of_jlite_op op ^ ", " ^ string_of_jlite_expr expr_left ^ ", " ^ string_of_jlite_expr expr_right ^ ")"
  | FieldAccess(expr, id) ->
    Printf.sprintf "FieldAccess(%s, %s)" (string_of_jlite_expr expr) (string_of_var_id id)
  | MdCall(expr, expr_lst) -> Printf.sprintf "MdCall(%s, %s)" (string_of_jlite_expr expr) (string_of_list string_of_jlite_expr expr_lst)
  | ObjectCreate(cls_name) -> Printf.sprintf "ObjectCreate(%s)" cls_name
  | ThisWord -> "ThisWord"
  | NullWord -> "NullWord"
  | _ -> failwith "string_of_jlite_expr: OOPS! no match!"

let rec string_of_jlite_stmt stmt =
  match stmt with
  | PrintStmt expr -> "PrintStmt(" ^ string_of_jlite_expr expr ^ ")"
  | AssignStmt(var_id, expr) -> "AssignStmt(" ^ string_of_var_id var_id ^ ", " ^ string_of_jlite_expr expr ^ ")"
  | WhileStmt(expr, stmts) -> Printf.sprintf "WhileStmt(%s, %s)"
    (string_of_jlite_expr expr) (string_of_list string_of_jlite_stmt stmts)
  | IfStmt(expr, stmts_if, stmts_else) -> Printf.sprintf "IfStmt(%s, %s, %s)"
    (string_of_jlite_expr expr) (string_of_list string_of_jlite_stmt stmts_if)
    (string_of_list string_of_jlite_stmt stmts_else)
  | MdCallStmt(expr) -> Printf.sprintf "MdCallStmt(%s)" (string_of_jlite_expr expr)
  | AssignFieldStmt(expr1, expr2) -> Printf.sprintf "AssignFieldStmt(%s,%s)" (string_of_jlite_expr expr1) (string_of_jlite_expr expr2)
  | ReturnStmt(expr) -> Printf.sprintf "ReturnStmt(%s)" (string_of_jlite_expr expr)
  | ReturnVoidStmt -> "ReturnVoidStmt"
  | _ -> failwith "string_of_jlite_stmt: OOPS"

let string_of_var_decl (ty, id) =
  Printf.sprintf "(%s %s)" (string_of_jlite_type ty) (string_of_var_id id)

(* let string_of_arg_decl arg_decl = "" *)

let string_of_md_decl { jliteid; ir3id; rettype; params; localvars; stmts } =
  "md_decl { jliteid: " ^ string_of_var_id jliteid ^";\n\t" ^
            "ir3id:" ^ string_of_var_id ir3id^ ";\n\t" ^
            "rettype:" ^ string_of_jlite_type rettype ^ ";\n\t" ^
            "params:" ^ string_of_list string_of_var_decl params ^ ";\n\t" ^
            "localvars:" ^ string_of_list string_of_var_decl localvars ^ ";\n\t" ^
            "stmts:" ^ string_of_list string_of_jlite_stmt stmts ^ ";\n\t" ^
          "}"

let string_of_class_main ~cls_main:(cls_name, main_md_decl) =
  Printf.sprintf "MainClass(\n%s, \n%s)"
    cls_name (string_of_md_decl main_md_decl)

let string_of_class_decl (cls_name, var_decls, md_decls) =
  Printf.sprintf "ClassDecl(%s,\n%s,\n%s)" cls_name
    (string_of_list string_of_var_decl var_decls) (string_of_list string_of_md_decl md_decls)

let string_of_jlite_program ~prog:(main_cls, classes) =
  Printf.sprintf "Program(\n(%s),\n%s)"
    (string_of_class_main ~cls_main:main_cls) (string_of_list string_of_class_decl classes)
