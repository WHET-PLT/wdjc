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

(* need to figure out what the java library constructs are
   that could complicate this *)
let ftype_string ftype = 
  match ftype with
    Int -> "int"
  | Note -> "note"
  | Chord -> "chord"
  | Track -> "track"
  | Rest -> "rest"
  
let paramlist_string formals =

let vdecllist_string locals = 
  
let stmtlist_string body =
  
(* expressions *)
(* statements *)
(* modifs and note attrs and ops? *)