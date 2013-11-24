open Ast

module StringMap = Map.Make(String)

type env = {
	functions:	string list StringMap.t;
	globals:	string StringMap.t;
	locals:		string StringMap.t;
}

let string_of_vartype = function
   Int -> "int"
   | Note -> "note"
   | Chord -> "chord"
   | Track -> "track"

(*map.find: returns the value associated with a key*)
(**)
let get_variable vname env = 
	try StringMap.find vname env.locals
	with Not_found -> try StringMap.find vname env.globals
	with Not_found -> raise (Failure ("undeclared variable " ^ vname))

let get_function fname env =
	try StringMap.find fname env.functions
	with Not_found -> raise (Failure ("undeclared variable " ^ fname))

(*map.mem: checks if value exists for a given key*)
(*adds to map a local var if not found*)
let add_local var_type name env =
	if StringMap.mem name env.locals then StringMap.empty
	else StringMap.add name (string_of_vartype vtype) env.locals

(*adds to map a global var if not found*)
let add_global var_type name env =
	if StringMap.mem name env.globals then StringMap.empty
	else StringMap.add name (string_of_vartype vtype) env.globals

(*adds to map a function, formals, and return type if not found*)
let add_function fname return formals env =
	if StringMap.mem name env.functions then StringMap.empty
	else let fmls = List.map get_type formals in
	StringMap.add name (string_of_vartype (return) :: fmls) env.functions

let get_type = function
	var -> string_of_vartype var.vartype

(*gets sast types*)
let ast_to_sast_type = function
   Ast.Int -> Sast.Int
   | Ast.Note -> Sast.Note
   | Ast.Chord -> Sast.Chord
   | Ast.Track -> Sast.Track
   | _ -> raise (Failure ("Mismatch type"))

