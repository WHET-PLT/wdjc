open Ast

module StringMap = Map.Make(String)

type env = {
	functions:	string list Stringmap.t;
	globals:	string Stringmap.t;
	locals:		string Stringmap.t;
}

let string_of_vartype = function
   Int -> "int"
   | Note -> "note"
   | Chord -> "chord"
   | Track -> "track"

let variable vname env = 
	try Stringmap.find vname env.locals
	with Not_found -> try Stringmap.find vname env.globals
	with Not_found -> raise (Failure ("undeclared variable " ^ vname))

let function fname env =
	try Stringmap.find fname env.functions
	with Not_found -> raise (Failure ("undeclared variable " ^ fname))

let add_local var_type name env =
	if Stringmap.mem name env.locals then Stringmap.empty
	else Stringmap.add name (string_of_vartype vtype) env.locals

let add_global var_type name env =
	if Stringmap.mem name env.globals then Stringmap.empty
	else Stringmap.add name (string_of_vartype vtype) env.globals

let get_type = function
	var -> string_of_vartype var.vartype

let add_function fname return formals env =
