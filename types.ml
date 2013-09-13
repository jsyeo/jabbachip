type class_name = string

type jlite_type =
  | IntT
  | BoolT
  | StringT
  | ObjectT of class_name
  | VoidT
  | Unknown

type jlite_op =
  | BooleanOp of string
  | RelationalOp of string
  | ArithmeticOp of string
  | UnaryOp of string

type var_id =
  | SimpleVarId of string
  | TypedVarId of typed_var_id
and typed_var_id = string * jlite_type * int

type jlite_expr =
  | UnaryExpr of jlite_op * jlite_expr
  | BinaryExpr of jlite_op * jlite_expr * jlite_expr
  | FieldAccess of jlite_expr * var_id
  | ObjectCreate of class_name
  | MdCall of jlite_expr * (jlite_expr list)
  | BoolLiteral of bool
  | IntLiteral of int
  | StringLiteral of string
  | ThisWord
  | NullWord
  | Var of var_id
  | TypedExpr of jlite_expr * jlite_type
and jlite_stmt =
  | IfStmt of jlite_expr * (jlite_stmt list) * (jlite_stmt list)
  | WhileStmt of jlite_expr * (jlite_stmt list)
  | ReadStmt of var_id
  | PrintStmt of jlite_expr
  | AssignStmt of var_id * jlite_expr
  | AssignFieldStmt of jlite_expr * jlite_expr
  | MdCallStmt of jlite_expr
  | ReturnStmt of jlite_expr
  | ReturnVoidStmt

type var_decl = jlite_type * var_id
and md_decl =
{
  jliteid: var_id;
  mutable ir3id: var_id;
  rettype: jlite_type;
  params: (var_decl list);
  localvars: (var_decl list);
  stmts: (jlite_stmt list)
}
and class_decl = class_name * (var_decl list) * (md_decl list)
and class_main = class_name * md_decl
and jlite_program = class_main * (class_decl list)
