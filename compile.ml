open Sast

(* need ast to java main body string *)
(* need body string to file ('java') *)

let function_string ftype fname formals locals body =
  let jtype = ftype_string ftype
  and jname = fname_string fname
  and jformals = paramlist_string formals
  and jlocals = vdecllist_string locals
  and jbody = stmtlist_string body in
  fprintf java "public %s %s(%s) { %s %s }" jtype jname jformals jlocals jbody; 


let ftype_string ftype =
  
let fname_string fname =
  
let paramlist_string formals =

let vdecllist_string locals = 
  
let stmtlist_string body =
  
(* expressions *)
(* statements *)
(* modifs and note attrs and ops? *)