open sast

(* need ast to java main body string *)
(* need body string to file ('java') *)

(* let function_string ftype fname formals locals body =
  let jtype = ftype_string ftype
  and jname = fname
  and jformals = paramlist_string formals
  and jlocals = vdecllist_string locals
  and jbody = stmtlist_string body in
  fprintf java "public %s %s(%s) { %s %s }" jtype jname jformals jlocals jbody; 
 *)
(* need to figure out what the java library 
   constructs are; that could complicate this *)
(* let ftype_string ftype = 
  match ftype with
    Int -> "int"
  | Note -> "note"
  | Chord -> "chord"
  | Track -> "track"
  | Rest -> "rest" *)
  
(* TODO: break down expression/formals loop *)
(* let rec params_string formals = function
    [] -> ""
  | [t] -> expr_string t
  | f::t -> expr_string f ^ ", " ^ params_string t   *)
  

(* break down variable dec *)
 (* let vardecl_string loc = 
 (match loc.vType with
  Int -> "int "
| Note -> "note "
| Chord -> "chord "
| Track -> "track "
| Rest -> "rest ") ^ loc.vName  *)
   
(*variable dec loop breakdown, not sure about print statement yet*)
(* let rec vdecllist_string locals = function
  [] -> ""
  |[t] -> vardecl_string t
  | f::t -> vardecl_string f ^ ", " ^ vardecl_string t
   let jvariables = locals in
  java fprintf "%s" jvariables;  *)


  
(* let stmtlist_string body =
  
(* expressions *)
let rec expr_string = function
(* statements *)
let rec stmt_string = function *)

(* modifs and note attrs and ops? *)


let imports=
"import java.util.LinkedList;\n" ^
"import java.util.List;\n" ^
"import jm.JMC;\n" ^
"import jm.music.data.*;\n" ^
"import jm.utl.*;\n" 
"public class DJ{\n 
public static void main(Strings[] args){\n
Song newSong = new Song();
newSong.composeSong();// make this in later method. \n} 
public class Song implements JMC{ \n
"


(* New code based on AST Pretty Printing *)
(* 
let string_of_vdecl v = 
  (match v.vType with
    Int -> "int "
    | Note -> "Note " 
    | Chord -> "CPhase"
    | Track -> "Part "
    | Rest -> "Rest ") ^ v.vName in 

 *)
(* let rec string_of_expr = function
    Literal(l) -> (* string_of_int  *) l
  | Id(s) -> s
  | NOTE_CR(a, b, c, d) -> (* this is going to be different  *)
     (*  "(" ^ a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d ^ ")" *)
     "Note " ^ ID(NOTE_CR(a,,b,c,d)) ^ " = new Note (" ^ a ^ ", 1);\n"
     ID(NOTE_CR(a,,b,c,d)) ".setDuration(" ^ c ");\n"
     ID(NOTE_CR(a,,b,c,d)) ".setVolume(" ^ b ");\n"
     (* here is where we set instrument... putting on back burner until other stuff done *)


  | Rest(r) -> "Rest " ID(Rest(r)) ^ " = new Rest((double)" ^ r ^ ");\n"



  | ACCESSOR(a, b) -> 
      a ^ "." ^ (
      match b with
        Pitch -> "getPitch()" | Vol -> "getVolume()" (* | Instr -> "instr()" *) | Dur -> "getDuration()"
      ) 

  | Assign(id, expr) -> id ^ " = " ^ string_of_expr expr (* good as is *)

(* need to figure out to use ID and get the pitch array proper *)
  | CHORD_CR(note_list) -> 
      (* "(" ^ String.concat " : " note_list ^ ")" *)
   "CPhrase c = new CPhrase();\n" 
    ^ "double[] pitchArray = new double[" note_list.length "];\n" 
    ^ string_of_chord note_list 
    ^ "c.addChord(pitchArray, C);\n" 

  | Track(t) -> "private static " ^ string_of_vdecl t ^ " = new Part(" ^ (*this is were instrument fits in *)", 0, 0)'\n"
  ^ ID(t) ^ ".addCPhrase(" ^  CHORD_CR(t) ^ ");\n"

------
(* How does this work... notes, chords, all?  *)
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
      Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | Ser -> "." | Par -> ":" | Arrow -> "->") ^ " " ^
      string_of_expr e2 

  (*again, not sure about this section*)
  | Modifier(e1, modif) ->
      string_of_expr e1 ^
      (match modif with
      Vib -> "^" | Trem -> "~" | Bend -> "%" | Incr -> "++" | Decr -> "--") 
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
(*| Array*) 

string_of_chord creates array items for every note
let index=0
and acc="" in 
let rec string_of_chord acc index chordList  = function
[] -> ""
| head::tail->  (acc ^ "pitchArray[" ^ index ^"] = " ^ ACCESSOR(head, Pitch) ^"];\n") (index+1) string_of_chord tail;;


(* let rec sum list = match list with
    | [] -> 0
    | head::tail -> head + sum tail;;
sum : int list -> int = <fun>
# sum [1; 2; 3];; *)


  (* let rec vdecllist_string locals = function
  [] -> ""
  |[t] -> vardecl_string t
  | f::t -> vardecl_string f ^ ", " ^ vardecl_string t
   let jvariables = locals in
  java fprintf "%s" jvariables;  *) 


(*pretty print for stmts*)
(*TODO need to do loop*)
let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"; (*can stay the same*)
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  (*| Assign(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e
  | Vdecl(v) -> string_of_vdecl v ^ ";"*)
 (*| Loop*)


let string_of_fdecl fdecl =
   (match fdecl.rtype with
    Int -> "int "
    | Note -> "note "
    | Chord -> "chord "
    | Track -> "track "
    | Rest -> "rest ") ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

(*pretty print for program*)
let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)  *) 

(* ------------------------------------------------------------------------------------------------------------- *)

  (* new code based on new ast that emily showed me *)


(*pretty print for expr*)
(*TODO need to decide on arrays*)
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | NOTE_CR(a, b, c, d) ->
      (* "(" ^ a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d ^ ")" *)
      "new Note((double)" ^a^ ", " ^b^ ", " ^ c ^ ")"


  | REST_CR(r) -> "new Rest((double) " ^ string_of_int r ^ ")" (* should this really be string of literal or something? *)
  | ACCESSOR(a, b) -> 
      a ^ "." ^ (
      match b with
        Pitch -> "getFrequency()" | Vol -> "getVolume" |  Dur -> "getDuration()r"
      )

  | Assign(id, expr) -> string_of_expr id ^ " = " ^ string_of_expr expr

  | CHORD_CR(note_list) -> 
      (* !!!we are going to have an issue here because chord is actually a cPhrase *)
      (* !!!also going to have an issue with ID naming situation *)
      "ArrayList<Note> noteArrayList = new ArrayList<Note>(); "
      "new CPhrase("
      List.map (fun a ->  "noteArrayList.add(" ^ a ^ ") ") note_list
      name_CPhrase ^ ".add(noteArrayList);"  

(* What exactly is track.. track creation, because that's what I'm writing it as. also where is the instrument part*)
  | Track(t) -> "new Part("t", \"pianoTrack\", \"Piano\");" 


  (* the question is whether this makes sense complete. it will work for variable ints + ints but not notes etc *)
  (* can we write a function to figure out if note or int etc??? *)
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
      Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2


  (* again, not sure about this section * also are we talking about incr decr pitch? by how much? *)
  | Modifier(e1, modif) ->
      string_of_expr e1 ^
      (match modif with
      Vib -> " " |
      Trem -> " " | 
      Bend -> " " | 
      Incr -> ".setPitch((" ^ string_of_expr e1 ^".getPitch()) + 50)"  | 
      Decr -> ".setPitch((" ^ string_of_expr e1 ^".getPitch())  -50)")
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
(*| Array*) 

let string_of_vdecl v = 
  (match v.vType with
    Int -> "int "
    | Note -> "Note "
    | Chord -> "CPhrase "
    | Track -> "Part "
    | Rest -> "Rest ") ^ v.vName 

(*pretty print for stmts*)
(*TODO need to do loop*)
let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  (* | Assign(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e *)
  | Vdecl(v) -> string_of_vdecl v ^ ";\n"
  | Vinit(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e ^ ";\n"

 (*| Loop*)


let string_of_fdecl fdecl =
   "public " (match fdecl.rtype with
    Int -> "int "
    | Note -> "Note "
    | Chord -> "CPhrase "
    | Track -> "Part "
    | Rest -> "Rest "
    | _ -> "void") ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"


(*pretty print for program*)
let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)  

let finalImports = "\n} \n}"







