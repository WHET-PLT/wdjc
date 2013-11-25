open Sast

(* need ast to java main body string *)
(* need body string to file ('java') *)

let function_string ftype fname formals locals body =
  let jtype = ftype_string ftype
  and jname = fname_string fname
  and jformals = vdecllist_string formals
  and jlocals = vdecllist_string locals
  and jbody = stmtlist_string body in
  fprintf java "public %s %s(%s) { %s %s }" jtype jname jformals jlocals jbody; 
