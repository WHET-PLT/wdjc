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
	if StringMap.mem name env.globals then StringMap.empty
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

(* check binop operands - EMILY *)

(* STATEMENTS - EMILY *)
(* check statement *)
(* check statement list *)


(* EXPRESSIONS - ?? *)

(* FUNCTIONS  - TOM *)
(* check formal list *)
(* check function arguments *)

(* check function *)
(* this function will return the updated formals and body 
as per the abstract syntax tree, the return type, name and locals *)
let rec sc_function fn env = 
	match List.hd (List.rev fn.body) with
		(* WHAT IS RETURN _*)
		Return(_) -> 
			let env = 
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
	(* if no function,s return empty list *)
	[] -> []
	| h:t -> let f, e = (sc_function h env) in f::(sc_functions t e)

(* GLOBALS - EMILY *)
(* sem check global *)
let sc_global global env =
	let r = add_global global.vType global.vName env in
		(* r is the new env	*)
		if StringMap.is_empty r then raise (Failure ("global variable " ^ 
			global.vName ^ " is already defined."))
		(* update env with globals from r *)
		else let env = {locals = env.locals; globals = r; 
			functions = env.functions} in
		ast_to_sast_type global env

(* sem check global list *)
let sc_globals globals env =
	match globals with
	[] -> [] (* empty list of globals*)
	| h::t -> let g, e = (sc_global h env) in (g,e)::(check_globals t e) (* add global, env to a list.*)


(* semantically check program - Emily *)
let sc_program (globals, functions) =
	let env = 
		{ 	locals 	  = StringMap.empty;
			globals   = StringMap.empty;
			functions = StringMap.empty 
		} in 
	(* return list of each global with environment; last global has entire env *)
	let g = sc_globals globals env in
		(* make a list of globals *)
		let globals = List.map (fun global -> fst global) g in
		match g with
		(* no globals *)
			[] -> (globals, (sc_functions (List.rev functions) env))
			(* get environment from the last global 
			WHATS SND
			*)
			| _ -> let e = snd (List.hd (List.rev g)) in 
				(globals, (sc_functions (List.rev functions) env))
















