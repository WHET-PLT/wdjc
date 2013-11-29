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
	if typ = "" then raise (Failure ("undefined variable: " ^ name))
	else typ

(*get type for expressions - tom fill in?? maybe wait until we 
do expression checking? not sure if we went through that yet??*)
let get_expr_type name env = 




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
		return - return type
		formals - formal arguments
		env - environment stringmap

	Checks to see if the fname is in env's function list
	if not- it gets the types of the formals, adds:
		name, vartype of return, formals to environemt's function
*)
let add_function fname return formals env =
	if StringMap.mem name env.functions then StringMap.empty
	else let fmls = List.map get_type formals in
	StringMap.add name (string_of_vartype (return) :: fmls) env.funct`ions



(* SEMANTIC CHECKING FUNCTIONS *)

(* check binop operands - TOM *)

(* STATEMENTS - TOM *)
(* check statement *)
(* check statement list *)


(* EXPRESSIONS - ?? *)

(* FUNCTIONS  - EMILY *)
(* check formal list *)
(*checks function arguments, then updates env*)
let formals_checker env formal =
	let ret = add_local formal.varname formal.vartype env in
	if StringMap.is_empty ret then
	raise (Failure ("formals_checker: variable " ^ formal.varname ^ "is already defined"))
	else let env = {locals = ret; globals = env.globals; functions = env.functions } in
	convert_types formal, env
(* check function arguments *)


(* updates formals from cur context *)
let rec formals_update formals env =
	match formals with
	  [] -> []
	| h::t -> let frm, en = (formals_checker env hd) in (frm, en)::(formals_update en tl) 


(* sc_function
	returns updated formals + body
	returns type, name, locals
 *)
let rec sc_function fn env = 
	match List.hd (List.rev fn.body) with
		(* WHAT IS RETURN(_); do we need parenthesis? *)
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
			 *)
			let env_new =
				add_function fn.rtype fn.fname fn.formals fn.locals env in(*CHECK THIS order!*)
				if StringMap.is_empty env_new then raise (Failure ("function " 
					^ fn.fName ^ " is already defined."))
				else let env =
					{
						locals = env.locals;
						globals = env.globals;
						functions = env_new
					} in
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




