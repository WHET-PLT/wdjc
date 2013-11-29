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
  | [f] -> expr_string f
  | f::t -> expr_string f ^ ", " ^ params_string t  
  

let vdecllist_string locals = 
  
let stmtlist_string body =
  
(* expressions *)
let rec expr_string = function
(* statements *)
let rec stmt_string = function

(* modifs and note attrs and ops? *)


and params_to_string paramsList= 
  let paramsStringList = List.map gen_expr paramsList in
    let rec paramsListtoString = function
        [] -> ""
      | [a] -> sprintf("%s") a 
      | hd::tl -> (sprintf("%s,") hd)^paramsListtoString tl
    in paramsListtoString paramsStringList 