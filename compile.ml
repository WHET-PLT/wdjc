open Sast

(* need ast to java main body string *)
(* need body string to file ('java') *)

let function_string ftype fname formals locals body =
  let jtype = ftype_string ftype
  and jname = fname
  and jformals = paramlist_string formals
  and jlocals = vdecllist_string locals
  and jbody = stmtlist_string body in
  fprintf java "public %s %s(%s) { %s %s }" jtype jname jformals jlocals jbody; 

(* need to figure out what the java library 
   constructs are; that could complicate this *)
let ftype_string ftype = 
  match ftype with
    Int -> "int"
  | Note -> "note"
  | Chord -> "chord"
  | Track -> "track"
  | Rest -> "rest"
  
(* TODO: break down expression/formals loop *)
let rec params_string formals = function
    [] -> ""
  | [t] -> expr_string t
  | f::t -> expr_string f ^ ", " ^ params_string t  
  

(* break down variable dec *)
 let vardecl_string loc = 
 (match loc.vType with
  Int -> "int "
| Note -> "note "
| Chord -> "chord "
| Track -> "track "
| Rest -> "rest ") ^ loc.vName 
   
(*variable dec loop breakdown*)
let rec vdecllist_string locals = function
  [] -> ""
  |[t] -> vardecl_string t
  | f::t -> vardecl_string f ^ ";\n" ^ vardecl_string t
   let jvariables = locals in
  java fprintf "%s" jvariables; 


  
let stmtlist_string body =
  
(* expressions *)
let rec expr_string = function
(* statements *)
let rec stmt_string = function

(* modifs and note attrs and ops? *)



(* hila code *)



