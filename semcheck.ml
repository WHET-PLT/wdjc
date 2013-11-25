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

(* FUNCTIONS  - TOM *)
(* check formal list *)
(* check function arguments *)
(* check function *)
(* check function list *)

(*checks function arguments, then updates env*)
let formals_checker env formal =
	let ret = add_local formal.varname formal.vartype env in
	if StringMap.is_empty ret then
	raise (Failure ("formals_checker: variable " ^ formal.varname ^ "is already defined"))
	else let env = {locals = ret; globals = env.globals; functions = env.functions } in
	convert_types formal, env

(*updates formals from cur context*)
let rec formals_update env formals =
	match formals with
	  [] -> []
	| hd::tl -> let frm, en = (formals_checker env hd) in (frm, en)::(formals_update en tl) 

(*invokes a function and returns updated formals and block from env. Needs to also
update the symbol table for global variables*)
let functions_checker env func =

let rec functions_update env funcs = 

(* GLOBALS - EMILY *)
(* sem check global *)
(* sem check global list *)
(* semantically check program *)
















