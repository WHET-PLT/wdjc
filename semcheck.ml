(* 

	semcheck.ml

	- semantically checks the input file
		- checks for type errors
		- undefined variable & function errors
	-converts from ast to sast

*)
open Ast
open Sast

(*NOTE:
	map.find: returns the value associated with a key
	map.mem: checks if value exists for a given key
*)


module StringMap = Map.Make(String)

type env = {
	functions:	string list StringMap.t;
	globals:	string StringMap.t;
	locals:		string StringMap.t;
}


(* TYPE CONVERSIONS *)

(* var type -> string *)
let string_of_vartype = function
   Int -> "int"
   | Note -> "note"
   | Chord -> "chord"
   | Track -> "track"


(* ast -> sast type*)
let ast_to_sast_type = function
   Ast.Int -> Sast.Int
   | Ast.Note -> Sast.Note
   | Ast.Chord -> Sast.Chord
   | Ast.Track -> Sast.Track
   | _ -> raise (Failure ("Mismatch type"))

(* we may need a variable total conversion from
ast to sast *)

(*need for locals, formals, and global variabes*)
let convert_types tp = 
{
	Sast.vartype = ast_to_sast_type tp.vartype;
	Sast.varname = tp.varname;
}

(* TYPES - do we need this?
let get_type = function
	var -> string_of_vartype var.vartype
	*)

(* search for variable types by name; return type or exception *)
let get_variable_type name env = 
	let typ = get_variable_name name env in
	if typ = "" then raise (Failure ("wrong type used: " ^ name))
	else typ

let isnote name env =
	let typ = get_variable_type name env in
	if typ = "note" then name
	else raise (Failure ("wrong type used: " ^ name))
	
let ischord name env =
	let typ = get_variable_type name env in
	if typ = "chord" then name
	else raise (Failure ("wrong type used: " ^ name))

let istrack name env =
	let typ = get_variable_type name env in
	if typ = "track" then name
	else raise (Failure ("wrong type used: " ^ name))
		
		

(* HELPFUL FUNCTIONS TO GET AND ADD VARIABLES (GLOBAL & LOCAL), FUNCIONS TO ENVIRONMENT *)

(* 
	get_variable vname env
		vname - variable name
		env - environment stringmap
	Looks to find variable name in env's local list.
	If it doesn't find it, it checks the env's global list.
	If not found, raises error.
*)
let get_variable_name vname env = 
	try StringMap.find vname env.locals
	with Not_found -> try StringMap.find vname env.globals
	with Not_found -> raise (Failure ("undeclared variable " ^ vname))


(* 
	get_function vname env
		vname - function name
		env - environment stringmap
	Looks to find function name in env's function list.
	If not found, raises error.
*)
let get_function fname env =
	try StringMap.find fname env.functions
	with Not_found -> raise (Failure ("undeclared variable " ^ fname))


(*
	add_local var_type name env
		var_type - variable type
		name - variable name
		env - environment stringmap
	Checks to see if the name is in env's local list.
	If it doesn't contain it, it adds it to the env's local list.
*)
let add_local var_type name env =
	if StringMap.mem name env.locals then StringMap.empty
	else StringMap.add name (string_of_vartype vtype) env.locals

(*
	add_global var_type name env
		var_type - variable type
		name - gariable name
		env - environment stringmap

	Checks to see if the name is in the env's global list.
	If it dlesn't contain it, it adds it to the env's global list.
*)
let add_global var_type name env =
	(* if name exists in env.globals, return empty stringmap *)
	if StringMap.mem name env.globals then StringMap.empty
	(*  else; add to env.globals:
		key = name
		value = vartype 
	*)
	else StringMap.add name (string_of_vartype vtype) env.globals

(*
	CONFUSED ON THE GET_TYPE 
	add_function fname return formals env
		fname - function name
		rtype - return type
		formals - formal arguments
		env - environment stringmap

	Checks to see if the fname is in env's function list
	if not- it gets the types of the formals, adds:
		name, vartype of return, formals to environemt's function
*)
let add_function fname rtype formals env =
	if StringMap.mem name env.functions then StringMap.empty
	else let fmls = List.map get_type formals in
	(* weird parenthesis...*)
	StringMap.add name (string_of_vartype (return) :: fmls) env.functions
	(*Strinmap.add, parse locals, add to env*)


(* SEMANTIC CHECKING FUNCTIONS *)


(* check binop operands - TOM *)

(*checks binop types. so far, we can do an op to two ints. 
  need to decide what types can be binop'd and how*)
let get_binop_expr_type t1 t2 = 
	if t1 = "int" && t2 = "int" then "int" else
	(*consideration for Ser and Par*)
	(*if t1 = "note" && t2 = "chord" then "chord" else
	if t1 = "chord" && t2 = "note" then "chord" else
	if t1 = "chord" && t2 = "track" then "track" else
	if t1 = "track" && t2 = "chord" then "track" else*)
	if t1 = "chord" && t2 = "chord" then "track" else (*serial*)
	if t1 = "note" && t2 = "note" then "chord" else (*parallel*)
	raise (Failure ("illegal operation types"))


(*Serial: combine chords to make a track. Can combine notes to make chord.
  Parallel: combine chords to make a track.
Serial (.)
track  = chord.chord...;
Parallel(:)
chord = note(:note.....);

*))
let sc_binop e1 o e2 =
	let expr_t = get_binop_expr_type (snd e1) (snd e2) in
	(match o with
	  Ast.Add -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Add, fst e2), "int") else
		  raise (Failure ("type error: add"))
	| Ast.Sub -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Sub, fst e2), "int") else
		  raise (Failure ("type error: sub"))
	| Ast.Mult -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Mult, fst e2), "int") else
		  raise (Failure ("type error: mult"))
	| Ast.Div -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Div, fst e2), "int") else
		  raise (Failure ("type error: div"))
	| Ast.Equal -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Equal, fst e2), "int") else
		  raise (Failure ("type error: equal"))
	| Ast.Neq -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Neq, fst e2), "int") else
		  raise (Failure ("type error: neq"))
	| Ast.Geq -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Geq, fst e2), "int") else
		  raise (Failure ("type error: geq"))
	| Ast.Leq -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Leq, fst e2), "int") else
		  raise (Failure ("type error: leq"))
	| Ast.Greater -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Greater, fst e2), "int") else
		  raise (Failure ("type error: greater"))
	| Ast.Less -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Less, fst e2), "int") else
		  raise (Failure ("type error: less"))
	| Ast.Ser -> if expr_t = "track" then (Sast.Binop(fst e1, Sast.Ser, fst e2), "track") else
		  raise (Failure ("type error: ser"))
	| Ast.Par -> if expr_t = "chord" then (Sast.Binop(fst e1, Sast.Par, fst e2), "chord") else
		  raise (Failure ("type error: par"))
	| Ast.Arrow -> if expr_t = "int" then (Sast.Binop(fst e1, Sast.Arrow, fst e2), "int") else
		  raise (Failure ("type error: arrow"))

	)

(*need to discuss actions of modifiers. do they go through every note in
  a chord? Can they be applied to just a single note?*)
let get_mod_expr_type t1 =
	if t1 = "note" then "note" else
	if t1 = "chord" then "chord" else
	if t1 = "track" then "track" else
	raise (Failure ("illegal modifier types"))

let sc_modifier e1 o =
	let expr_t = get mod_expr_type (snd e1) in
	(match o with
	  Ast.Vib ->
	| Ast.Trem ->
	| Ast.Bend ->
	| Ast.Incr ->
	| Ast.Decr -> 

	)

(*
	locals are now vdecls, vinits. Need to pass a fuction a stmt_list and 
	function will go through list to pull out vdecls/vinits.
	Implemented at 365, 375. 

	add_to_locals function
*)

(* STATEMENTS - TOM *)
(* check statement *)
(* check statement list *)
let rec stmt_checker env func = function
	  Ast.Block(stmt_list) -> (Sast.Block(stmt_list_checker env func stmt_list)), env
	  (*need to check expr eval*)
	| Ast.Expr(expr) -> (Sast.Expr( (expr_checker env expr))), env
	| Ast.Return(expr) -> let e = expr_checker env expr in
						if not(snd e = string_of_vartype func.return) then raise (Failure ("Illegal return type: func type and return type must match"))
						else (Sast.Return(fst e)), env
	| Ast.If(expr, stmt1, stmt2) -> (Sast.If((expr_checker env expr), (stmt_checker env func stmt1), (stmt_checker env func stmt2))), env
	| Ast.For(expr1, expr2, expr3, stmt) -> (Sast.For((expr_checker env expr1), (expr_checker env expr2), (expr_checker env expr3), (stmt_checker env func stmt))), env
	| Ast.While(expr, stmt) -> (Sast.While((expr_checker env expr), stmt_checker env func stmt)), env


let rec stmt_list_checker env func = 
	[] -> []
  | hd::tl -> let st, en = (stmt_checker env func hd) in st::(stmt_list_checker en func tl)


(* let rec sc_local_vars func env =  *)

(* check the expression type can be used for
 * the corresponding argument according to definition
 * return the new expression list in expr_t for sast *)
let sc_func_arg lst expr arg_t =
	if (snd expr) = arg_t then (fst expr)::lst else
	raise (Failure("function arguments do not match"))

(* returns expr + its type 
meat of this part taken from sast.ml
*)
let rec sc_expr env expr = function
	(* literal *)
	Ast.Literal(i) -> Sast.Literal(i)
	(* accessor *)
	| Ast.ACCESSOR(id, note_attr) -> Sast.ACCESSOR( (isnote id env), (sc_expr note_attr) ), "int"
	(* id *)
	| Ast.Id(i) -> Sast.Id(i), (get_variable_type i env)
	(* note creation *)
	| Ast.NOTE_CR(s1,s2,s3,s4) ->
	(*
		need to get the type of each expr/id
	| Ast.NOTE_CR(i1, i2, i3, i4) -> (Sast.NOTE_CR((get_variable_type i1 env), (get_variable_type i2 env), (get_variable_type i3 env), (get_variable_type i4 env))), "note"
	*)
	(* Rest *)
	| Ast.Rest(s) -> 
	(*  chord create *)
	(*list of notes*)
	| Ast.CHORD_CR(str_lst) ->
	(* track *)
	| Ast.Track(s) ->
	(* binop *)
 	| Ast.Binop(e1, op, e2) ->
 		sc_binop (sc_expr env e1) op (sc_sexpr env e2)
	(* Assign *)
	| Ast.Assign(s, exp) -> 
 	(* Call 
 	| Ast.call(func, expr_list) ->
 		let args = get_function func env in
 			(match args with
 				[] -> raise (Failure ("undefined function " ^ func))
 				| h::t -> let new_lst = 
 					try List.fold_left2 sc_func_arg [] (List.map (sc_expr env) expr_list)
 				with 
 			)
	*)
 	| Ast.Noexpr -> Sast.Noexpr, "void" (* do we even have void type *)





(* FUNCTIONS  - EMILY *)
(*checks function arguments, then updates env*)
let sc_formal formal env =
	(*fstrently, formals are var_decls*)
	let new_env = add_local formal.vName formal.vType env in
	if StringMap.is_empty new_env then
		raise (Failure ("formals_checker: variable " ^ formal.varname ^ "is already defined"))
	else let env = 
		{
			locals = new_env; 
			globals = env.globals; 
			functions = env.functions 
		} in
	convert_types formal, env
(* check function arguments *)


(* updates formals from fst context *)
let rec sc_formals formals env =
	match formals with
	  [] -> []
	| h::t -> let f, new_e = (sc_formal h env) in (f, e)::(sc_formals t new_e) 


(* sc_function
	returns updated formals + body
	returns type, name, locals
 *)
let rec sc_function fn env = 
	match List.hd (List.rev fn.body) with
		(* check there is a return statement at the end of the function *)
		Return(_) -> 
			(* updating this function's personal envirnment *)
			let local_env = 
				{
					locals = StringMap.empty;
					globals = env.globals;
					functions = env.functions;
				}
			(* fill up env_new with functions;
			change name possibly to something more intuitive
			new_fn_sm - new function stringmap
			 *)
			let new_func_sm =
				add_function fn.rtype fn.fname fn.formals fn.locals env in
				if StringMap.is_empty env_new then raise (Failure ("function " 
					^ fn.fName ^ " is already defined."))
				else let env =
					{
						locals = env.locals;
						globals = env.globals;
						functions = new_func_sm (* new function env *)
					} in
			(* check formal arguments with sc_formals 
			formals_env
				- returns formal list appended w/ new environment as tuples
			*)

			let f = sc_formals fn.formals env in
				let formals = List.map (fun formal -> fst formal ) f in
				(match f with
					(* empty, no formals *)
					[] -> let body = stmt_list_checker fn fn.body env in
						{
							Sast.rtype = ast_to_sast_type fn.rtype;
							Sast.fname = fn.fname;
							Sast.formals = formals; (* ie empty *)
							(* Change locals *)
							Sast.locals = List.map  ast_to_sast_type fn.locals;
							Sast.body = body
						}, env
					|_ -> let new_env = snd (List.hd (List.rev f)) in
						let body = stmt_list_checker fn fn.body new_env in
						{
							Sast.rtype = ast_to_sast_type fn.rtype;
							Sast.fname = fn.fname;
							Sast.formals = formals; (* ie empty *)
							(* Change locals *)
							Sast.locals = List.map  ast_to_sast_type fn.locals;
							Sast.body = body
						}, new_env
				)
			|_ -> raise (Failure ("The last statement must be a return statement"))
			(*let f = sc_formals fn.formals env i stopped fu nv stuff at ln 196*)


(* check function list *)
let rec sc_functions fns env =
	match fns with
	(* if no functions, return empty list *)
	[] -> []
	(* otherwise, go through and create a list of function, environment
	pairs; the last element in the list is the most up-to-date env *)
	| h:t -> let f, e = (sc_function h env) in (f, e)::(sc_functions t e)


(* TOM - I don't know what this is so I didn't want to change it *)
(*invokes a function and returns updated formals and block from env. Needs to also
update the symbol table for global variables*)
(*let functions_checker env func =

let rec functions_update env funcs = 
*)


(* GLOBALS - EMILY *)

(* sem check global *)
let sc_global global env = 
	(* add_global returns updated stringmap *)
	let new_global_sm = add_global global.vType global.vName env in
		(* if already exists in the add_global, don't add it; crash program *)
		if StringMap.is_empty new_global_sm then raise (Failure ("global variable " ^ 
			global.vName ^ " is already defined."))
		(* update env with globals from r *)
		else let env = 
			{
				locals = env.locals; 
				globals = new_global_sm;
				functions = env.functions
			} in
		(* 
		RETURN: global + env
		*)
		convert_types global, env 

(* sem check list of globals *)
let rec sc_globals globals env =
	match globals with
	(* empty list of globals*)
	[] -> [] 
	(* 
		- iterate through list of globals 
		- semantically check each individual global
		- (g, e) end up being pairs of globals + respective environments
		- the last (g,e) pair has env with info from all globals
	*)
	| h::t -> let g, e = (sc_global h env) in (g,e)::(sc_globals t e)


(* semantically check program - Emily *)
let sc_program (globals, functions) =
	(*  initialize empty env *)
	let env = 
		{ 	locals 	  = StringMap.empty;
			globals   = StringMap.empty;
			functions = StringMap.empty 
		} in 
	(* 
		sc_globals returns list: [(g1,e1), (g2,e2), (g3,e3)....(gn,en)]
		where g = global , e = environment
	*)
	let g = sc_globals globals env in
		(* make a list of globals *)
		(* note: fun = function pattern matching *)
		(* note: elementss returned are in form (g, e)
			-fst global returns g
			-snd global returns e
		*)

		let globals = List.map (fun global -> fst global) g in
			match g with
				(* no globals; thus our environment stays the same *)
				[] -> (globals, (sc_functions (List.rev functions) env))
				(* 
				e - most up-to-date environment with all globals
				*)
				| _ -> let new_env = snd (List.hd (List.rev g)) in 
					(globals, (sc_functions (List.rev functions) new_env))




