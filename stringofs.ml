open Types

let rec string_of_list pr lst =
  "[" ^
    begin
      match lst with
      | [] -> "]"
      | [x] -> pr x ^ "]"
      | (x::xs) -> pr x ^ "; " ^ string_of_list pr xs
    end

let string_of_jlite_type ty = ""

let string_of_jlite_op op =
  match op with
  | BooleanOp s -> s
  | RelationalOp s -> s
  | ArithmeticOp s -> s
  | UnaryOp s -> s

let string_of_var_id id =
  match id with
  | SimpleVarId s -> "SimpleVarId(" ^ s ^ ")"
  | TypedVarId s -> Printf.sprintf "TypedVarId(%s)" s

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

let string_of_var_decl var_decl = ""

let string_of_arg_decl arg_decl = ""

let string_of_md_decl { jliteid; ir3id; params; localvars; stmts } =
  "md_decl { jliteid: " ^ string_of_var_id jliteid ^";\n" ^
            "ir3id:" ^ string_of_var_id ir3id^ ";\n" ^
            "params:" ^ string_of_list string_of_var_decl params ^ ";\n" ^
            "localvars:" ^ string_of_list string_of_var_decl localvars ^ ";\n" ^
            "stmts:" ^ string_of_list string_of_jlite_stmt stmts ^ ";\n" ^
          "}"

let string_of_class_main cls_main = ""

let string_of_class_decl cls_decl = ""

let string_of_jlite_program prog = ""
