
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*     TypeChecking of Jlite programs *)
(*     A Simplified version  *)
(*     Submitting at your own risk  *)
(* ===================================================== *)

open Jlite_structs

(* Compare two variable ids *) 	
let compare_var_ids v1 v2 =
	match v1,v2 with
	| SimpleVarId id1, SimpleVarId id2 -> 
		((String.compare id1 id2) == 0)
	| SimpleVarId id1, TypedVarId (id2,t,s) -> 
		((String.compare id1 id2) == 0)	
	| TypedVarId (id1,t,s), SimpleVarId id2 -> 
		((String.compare id1 id2) == 0)		
	| TypedVarId (id1,t1,s1), TypedVarId (id2,t2 ,s2) ->
		((String.compare id1 id2) == 0) && (s1 == s2)
		
(* Find the declared type of a variable *) 		
let rec find_var_decl_type 
	(vlst: var_decl list) (vid:var_id) =
  match vlst with
    | [] -> (Unknown, SimpleVarId "") 
    | (t,v)::lst -> 
		if (compare_var_ids v vid) 
		then (t,v) 
		else (find_var_decl_type lst vid)

(* Check if a variable id exists *)
let exists_var_id 
	(vlst: var_decl list) (vid: var_id) : bool =
	let is_eq_id ((t,v): var_decl):bool =
		(compare_var_ids v vid) 
	in (List.exists is_eq_id vlst) 	

(* Check if the declaration of a class exists *) 			  
let exists_class_decl 
	((cm,clst): jlite_program) (cid:class_name) =
	(List.exists 
		(fun (cname,cvars,cmtd) ->
			(String.compare cname cid) == 0)
	clst)

(* Annotate a list of variable declarations with their scope *)	
let rec create_scoped_var_decls
	(vlst: var_decl list) (scope:int) =
	let helper ((vt,vid):var_decl) =
		match vid with
		| SimpleVarId id -> 
			(vt, TypedVarId (id, vt, scope))
		| TypedVarId (id,t,s) -> 
			(vt,TypedVarId (id, vt, scope))
	in (List.map helper vlst)
  
(* Type check a list of variable declarations 
  1) Determine if all object types exist
  2) Find and return duplicate variable names -- TODO	

    This type check function is a simple version in 
    which there is only one variable declaration in 
    the declaration list
	The function returns the 
	 typecheck result (true or false) and an error message.
*)  
let rec type_check_var_decl_list
	(p: jlite_program) 
	(vlst: var_decl list) =
	let rec helper 
		(vlst: var_decl list) :jlite_type list =
		match vlst with
		| (typ,vid)::[] -> 
			begin
			match typ with
			| ObjectT cname -> 
	        (* check if the declared class name exists *)
				if (exists_class_decl p cname) 
					then []
					(* return the undefined type *)
					else typ::[] 
			(* Primitive type *)
			| _ -> ( [] ) 
			end
	in match ( helper vlst) with
		| [] ->  (true,"")
		| lst -> (false, ("Undefined types: " 
				^ (string_of_list lst string_of_jlite_type ",")))

 
(* Type check a list of method declarations 
  1) Determine if there is illegal overloading
  2) Find and return overloaded method names	

    This simplified version always returns true
	---  TODO  ---
*)  
let rec type_check_md_overloading 
	(classid: class_name) (mdlst: md_decl list) = (true,"") 
				
(* Type check an expression *)
(* Return the type of the Expression 
    and a new TypedExpession *)  
let rec type_check_expr 
	(p: jlite_program)(env: var_decl list) 
	(classid: class_name) (exp:jlite_exp) = 
	let rec helper e 
	: (jlite_type * jlite_exp) =
		match e with
		| BoolLiteral v -> (BoolT, e)
		| IntLiteral v -> (IntT, e)
		| StringLiteral v -> (StringT, e)
		| ThisWord -> 
			((ObjectT classid), TypedExp (e,(ObjectT classid)))
		| NullWord -> 
			((ObjectT "null") , TypedExp (e,(ObjectT "null")))
		| Var v -> 
			let (vtyp,vid) =(find_var_decl_type env v) in
			(vtyp, TypedExp (Var vid,vtyp)) 
		| ObjectCreate c -> 
			if (exists_class_decl p c) 
			then ((ObjectT c), TypedExp(e,(ObjectT c)))
			else (Unknown, e)
		| _ -> (Unknown, e) 
		 (* Handle other expresion types ---- TODO ---- *)
	  in  helper exp

(* Type check a list of statements and determine the return type.
   Exceptions are thrown when a statement does not type check 
   and when dead code is found
*)  
let rec type_check_stmts 
	(p: jlite_program)(env: var_decl list) 
	(classid: class_name) 
	(mthd: md_decl) 
	(stmtlst:jlite_stmt list)
	(rettype: jlite_type option) 
	: (jlite_type option *(jlite_stmt list))  =
	match stmtlst with
	| [] -> (rettype,[])
	| s::tail_lst -> 
		let rec helper s 
		: (jlite_type option * jlite_stmt) =
		match s with
		| ReturnStmt e ->  
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			begin
			match expr_type with
			| Unknown -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Return expression fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  (Some expr_type, ReturnStmt exprnew)
			end
		| ReturnVoidStmt ->  
			(Some VoidT,ReturnVoidStmt)
		| ReadStmt id -> 
			let (idtype,scopedid) = (find_var_decl_type env id) in
			begin
			match idtype with
			| ObjectT _ | Unknown  -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Read statement fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  (None,ReadStmt scopedid)
			end
		| PrintStmt e -> 
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			begin
			match expr_type with
			| Unknown | ObjectT _ -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Statement fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  (None,PrintStmt exprnew)
			end
		(* _ -> Handle other Statement types
		  ---- TODO ---- *)
	  in let (newrettype,newstmt) = ( helper s) in
	  match newrettype,tail_lst with
		| Some t, head::tail -> 
			failwith 
			("\nType-check error in " ^ classid ^ "." 
			 ^ string_of_var_id mthd.jliteid 
			 ^ ". Dead Code:\n" 
			 ^ (string_of_list tail_lst string_of_jlite_stmt "\n" ^ "\n")) 
		| _,_ ->  
			let (rettype,stmts) = 
				(type_check_stmts p env classid mthd tail_lst newrettype) in
				(rettype,(newstmt::stmts))
  
(* TypeCheck a JLite Method Declaration *)
let type_check_mthd_decl p env cname m : md_decl = 
	let mthdenv = 
		List.append m.params m.localvars in 
	let (retval, errmsg) = 
		(type_check_var_decl_list p mthdenv)
	in if (retval == false) 
		then failwith 
		 ("\nType-check error in " ^ cname ^ "." 
		  ^ string_of_var_id m.jliteid 
		  ^ " parameter or local variables declarations.\n"
		  ^ errmsg ^ "\n")
		else
		let scopedEnv = List.append 
				(create_scoped_var_decls mthdenv 2) env in 
		(* TypeCheck the body of the method *)
			let (rettyp,newstmts) = 
				(type_check_stmts p scopedEnv cname m m.stmts None) in
		(* TypeCheck the return type of the method *)
			let _ = match rettyp,m.rettype with
			| None, VoidT -> true
			| Some VoidT, VoidT -> true
			| None, t -> 
				failwith 
				("\nType-check error in " ^ cname ^ "." 
				^ string_of_var_id m.jliteid 
				^ ". This method must return a result of type "
				^ string_of_jlite_type m.rettype ^ ". \n")
			| Some (ObjectT t1), (ObjectT t2) -> 
				if ((String.compare t1 t2) != 0) 
				then failwith 
					("\nType-check error in " ^ cname ^ "." 
					^ string_of_var_id m.jliteid 
					^ ". Type mismatch. Return type of method " 
					^ "is different from declared type "
					^ string_of_jlite_type m.rettype ^ t1 ^ ". \n")
				else true
			| Some t1, t2 -> 
				if (t1!= t2) 
				then failwith 
					("\nType-check error in " ^ cname ^ "." 
					^ string_of_var_id m.jliteid 
					^ ". Type mismatch. Return type of method "
					^ "is different from declared type "
					^ string_of_jlite_type m.rettype 
					^ string_of_jlite_type t1 ^ ". \n")
				else true
			in { m with stmts=newstmts;
				}

(* TypeCheck a JLite Program. 
   Return a new JLite Program 
   where expressions are annotated with types
*)
let type_check_jlite_program  
	(p:jlite_program) : jlite_program=
	let type_check_class_main 
		((cname,mmthd):class_main ) =
		(cname,(type_check_mthd_decl p [] cname mmthd )) in
	let rec type_check_class_decl 
		((cname,cvars,cmthds):class_decl) =
		(* TypeCheck field declarations *)
		let (retval, errmsg) = 
			(type_check_var_decl_list p cvars) in
		if (retval==false) then 
			failwith 
			("\nType-check error in " ^ cname 
			^ " field declarations." ^ errmsg ^ "\n")
		(* TypeCheck methods overloading *)
		else let (retval, errmsg) = 
			(type_check_md_overloading cname cmthds) in
			if (retval==false) then 
				failwith 
				("\nType-check error in " ^ cname 
				^ " method declarations." ^ errmsg ^ "\n")
			(* TypeCheck method declarations *)
			else let env = (create_scoped_var_decls cvars 1) 
			in (cname, cvars, 
				List.map 
				(type_check_mthd_decl p env cname) cmthds
				)
	in 
	begin
		let (mainclass, classes) = p in 
		let newmain = 
			(type_check_class_main mainclass) in
		let newclasses=
			(List.map type_check_class_decl classes) in
		(newmain, newclasses)
	end
