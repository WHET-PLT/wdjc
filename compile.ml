open Sast

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


let imports =
  "import java.util.*;\n" ^
  "import jm.JMC;\n" ^
  "import jm.music.data.*;\n" ^
  "import jm.util.*;\n" ^
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
let rec string_of_expr_t ?(opt_name="null") expr = 
  match expr with
    Literal_t(l) -> string_of_int l
  | Id_t(s) -> s
  | NOTE_CR_t(a, b, c) ->
      (* "(" ^ a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d ^ ")" *)
      "new Note((double)" ^ string_of_expr_t a^ ", " ^  string_of_expr_t b^ ", " ^  string_of_expr_t c ^ ")"


  | REST_CR_t(r) -> "new Note(( REST, " ^ string_of_expr_t r ^ ")" (* should this really be string of literal or something? *)
  | ACCESSOR_t(a, b) -> 
      string_of_expr_t a ^ "." ^ (
      match b with
        Pitch_t -> "getFrequency()" | Vol_t -> "getVolume()" |  Dur_t -> "getDuration()"
      )

  | Assign_t(id, expr) -> string_of_expr_t id ^ " = " ^ string_of_expr_t ~opt_name:(string_of_expr_t id) expr (*implementation of optional name param *)

  | CHORD_CR_t(note_list) -> 
      (* !!!we are going to have an issue here because chord is actually a cPhrase *)
      (* !!!also going to have an issue with ID naming situation *)
      " new CPhrase();\n" ^
      "ArrayList<Note> noteArrayList = new ArrayList<Note>();\n" ^

      (* String.concat "\nnoteArrayList.add(" (List.map string_of_expr_t note_list) ^ ");\n" ^ *)
      (* hack??? Assumes that there are at least 2 notes here...*)
     "noteArrayList.add("^
      String.concat  ");\nnoteArrayList.add(" (List.map string_of_expr_t note_list) ^ ");\n" ^ 
      (* List.map (fun note -> "noteArrayList.add(" ^ string_of_expr_t a ^ ")\n") note_list *)
      opt_name ^ ".add(noteArrayList);\n"

      (*List.map (fun a ->  "noteArrayList.add(" ^ string_of_expr_t a ^ ")\n")  note_list
      name_CPhrase ^ ".add(noteArrayList);" *)

(* What exactly is track.. track creation, because that's what I'm writing it as. also where is the instrument part*)
  | TRACK_CR_t(track_list) ->  
      (* "new Part( \"" ^ string_of_expr_t t ^ "\");"  *)
      " new Part();\n" ^
      "ArrayList<> phraseArrayList = new ArrayList<>();" 
  (* Create function for this here. figure out if arraylist will work with this..  *)


  (* the question is whether this makes sense complete. it will work for variable ints + ints but not notes etc *)
  (* can we write a function to figure out if note or int etc??? *)
  | Binop_t(e1, o, e2) ->
      string_of_expr_t e1 ^ " " ^
      (match o with
      Add_t -> "+" | Sub_t -> "-" | Mult_t -> "*" | Div_t -> "/"
      | Equal_t -> "==" | Neq_t -> "!="
      | Less_t -> "<" | Leq_t -> "<=" | Greater_t -> ">" | Geq_t -> ">=" 
      | Ser_t -> "" | Par_t -> "" | Arrow_t -> "") ^ " " ^
      string_of_expr_t e2


  (* again, not sure about this section * also are we talking about incr decr pitch? by how much? *)
  | Modifier_t(e1, modif) ->
      string_of_expr_t e1 ^
      (match modif with
      Vib_t -> " " |
      Trem_t -> " " | 
      Incr_t -> ".setPitch((" ^ string_of_expr_t e1 ^".getPitch()) + 50)"  | 
      Decr_t -> ".setPitch((" ^ string_of_expr_t e1 ^".getPitch())  -50)")
  | Call_t(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr_t el) ^ ")"
  | Noexpr_t -> ""
(*| Array*) 

let string_of_vdecl_t v = 
  (match v.vType_t with
    Int_t -> "int "
    | Note_t -> "Note "
    | Chord_t -> "CPhrase "
    | Track_t -> "Part "
    | Rest_t-> "Rest ") ^ v.vName_t 

(*pretty print for stmts*)
(*TODO need to do loop*)
let rec string_of_stmt_t = function
    Block_t(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt_t stmts) ^ "}\n"
  | Expr_t(expr) -> string_of_expr_t expr ^ ";\n";
  | Return_t(expr) -> "return " ^ string_of_expr_t expr ^ ";\n";
  | If_t(e, s, Block_t([])) -> "if (" ^ string_of_expr_t e ^ ")\n" ^ string_of_stmt_t s
  | If_t(e, s1, s2) ->  "if (" ^ string_of_expr_t e ^ ")\n" ^
      string_of_stmt_t s1 ^ "else\n" ^ string_of_stmt_t s2
  | For_t(e1, e2, e3, s) ->
      "for (" ^ string_of_expr_t e1  ^ " ; " ^ string_of_expr_t e2 ^ " ; " ^
      string_of_expr_t e3  ^ ") " ^ string_of_stmt_t s
  | While_t(e, s) -> "while (" ^ string_of_expr_t e ^ ") " ^ string_of_stmt_t s
  (* | Assign(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e *)
  | Vdecl_t(v) -> string_of_vdecl_t v ^ ";\n"
  | Vinit_t(v, e) -> string_of_vdecl_t v ^ " = " ^ string_of_expr_t e ^ ";\n"

 (*| Loop*)


let string_of_fdecl_t fdecl =
   "public " ^ (match fdecl.rtype_t with
    Int_t -> "int "
    | Note_t -> "Note "
    | Chord_t -> "CPhrase "
    | Track_t -> "Part "
    | Rest_t -> "Rest "
    | _ -> "void") ^ fdecl.fname_t ^ "(" ^ String.concat ", " (List.map string_of_vdecl_t fdecl.formals_t) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt_t fdecl.body_t) ^
  "}\n"


(*pretty print for program*)
let string_of_program_t (vars, funcs) =
  "\n" ^ imports ^ 
  String.concat "" (List.map string_of_vdecl_t vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl_t funcs)  

let finalImports = "\n} \n}"


(* look over how were doing the song funcitons... *)






