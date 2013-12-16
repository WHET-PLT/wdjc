open Sast
open Printf

let imports =
  "import java.util.*;\n" ^
  "import jm.JMC;\n" ^
  "import jm.music.data.*;\n" ^
  "import jm.util.*;\n\n" ^
  "public class DJ implements JMC{\n" 
  

(* ------------------------------------------------------------------------------------------------------------- *)

  (* new code based on new ast that emily showed me *)

let rec string_of_expr_list v_name expr_list = 
  match expr_list with
  [] -> []
  | hd::tl -> let string_expr_list = (string_of_expr_t v_name hd) in string_expr_list::(string_of_expr_list v_name tl) (* returns SAST body which is a SAST stmt list *)

(*pretty print for expr*)
(*TODO need to decide on arrays*)
and string_of_expr_t v_name ex = 
  match ex with
    Literal_t(l) -> l
  | Id_t(s) -> s
  | NOTE_CR_t(a, b, c) ->
      (* "(" ^ a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d ^ ")" *)
      "new Note((double)" ^ string_of_expr_t "junk" a^ ", " ^  string_of_expr_t "junk" b^ ", (int) " ^  string_of_expr_t "junk" c ^ ");"


  | REST_CR_t(r) -> "new Note(( REST, " ^ string_of_expr_t "junk" r ^ ")" (* should this really be string of literal or something? *)
  | ACCESSOR_t(a, b) -> 
      (string_of_expr_t "junk" a) ^ "." ^ (
      match b with
        Pitch_t -> "getFrequency()" | Vol_t -> "getDynamic()" |  Dur_t -> "getDuration()"
      )

  | Assign_t(id, expr) -> (string_of_expr_t "junk" id) ^ " = " ^ (string_of_expr_t (string_of_expr_t "junk" id) expr)

  | CHORD_CR_t(note_list) -> 
    let notes = string_of_expr_list "junk" note_list in
      (* !!!we are going to have an issue here because chord is actually a cPhrase *)
      (* !!!also going to have an issue with ID naming situation *)
      " new CPhrase();\n" ^
      (* "ArrayList<Note> noteArrayList = new ArrayList<Note>();\n" ^ *)

      (* String.concat "\nnoteArrayList.add(" (List.map string_of_expr_t note_list) ^ ");\n" ^ *)
      (* hack??? Assumes that there are at least 2 notes here...*)
     (* "noteArrayList.add("^ *)
      (* String.concat  ");\nnoteArrayList.add(" notes ^ ");\n" ^  (**FLAG!!!!***) *)
    "Note [] notes_array = {" ^ String.concat ", " notes ^ "};\n" ^ 
      (* v_name ^ ".add(noteArrayList);\n" *)
      v_name^ ".addChord(notes_array);"

      (*List.map (fun a ->  "noteArrayList.add(" ^ string_of_expr_t a ^ ")\n")  note_list
      name_CPhrase ^ ".add(noteArrayList);" *)

(* What exactly is track.. track creation, because that's what I'm writing it as. also where is the instrument part*)
  | TRACK_CR_t(track_list) ->  
      (* "new Part( \"" ^ string_of_expr_t t ^ "\");"  *)
      " new Part();\n" ^
      (* "ArrayList<Phrase> chordArrayList = new ArrayList<Phrase>();\n" ^ (*same as cphrase *) *)
       v_name ^ ".addCPhrase("^   String.concat " , " (string_of_expr_list "junk" track_list) ^ ");\n"
  (* Create function for this here. figure out if arraylist will work with this..  *)
  | SCORE_CR_t(track_list) ->  
      (* "new Part( \"" ^ string_of_expr_t t ^ "\");"  *)
      " new Score();\n" ^
      (* "ArrayList<Part> phraseArrayList = new ArrayList<Part>();\n" ^  (*same as cphrase *) *)
      v_name ^ ".addPart(" ^ String.concat " , " (string_of_expr_list "junk" track_list) ^ ");"

  (* the question is whether this makes sense complete. it will work for variable ints + ints but not notes etc *)
  (* can we write a function to figure out if note or int etc??? *)
  | Binop_t(e1, o, e2) ->
      string_of_expr_t "junk" e1 ^ " " ^
      (match o with
      Add_t -> "+" | Sub_t -> "-" | Mult_t -> "*" | Div_t -> "/"
      | Equal_t -> "==" | Neq_t -> "!="
      | Less_t -> "<" | Leq_t -> "<=" | Greater_t -> ">" | Geq_t -> ">=" 
      | Ser_t -> "" | Par_t -> "" ) ^ " " ^
      string_of_expr_t "junk" e2


  (* again, not sure about this section * also are we talking about incr decr pitch? by how much? *)
  | Modifier_t(e1, modif) ->
      string_of_expr_t "junk" e1 ^
      (match modif with
      Vib_t -> " " |
      Trem_t -> " " | 
      Incr_t -> ".setPitch((" ^ string_of_expr_t "junk" e1 ^".getPitch()) + 50)"  | 
      Decr_t -> ".setPitch((" ^ string_of_expr_t "junk" e1 ^".getPitch())  -50)")
  | Call_t(f, el) ->
        let calls = string_of_expr_list "junk" el in
      f ^ "(" ^ String.concat ", " calls ^ ")"
  | Noexpr_t -> ""
(*| Array*) 

let string_of_vdecl_t v = 
  (match v.vType_t with
    Double_t -> "double "
    | Note_t -> "Note "
    | Chord_t -> "CPhrase "
    | Track_t -> "Part "
    | Rest_t-> "Rest "
    | Score_t -> "Score ") ^ v.vName_t 

let string_of_vdecl_name_t v = v.vName_t 


let rec string_of_stmt_list f_name stmt_list = 
  match stmt_list with
  [] -> []
  | hd::tl -> let string_stmt_list = (string_of_stmt_t f_name hd) in string_stmt_list::(string_of_stmt_list f_name tl) (* returns SAST body which is a SAST stmt list *)


(*pretty print for stmts*)
(*TODO need to do loop*)
and string_of_stmt_t f_name statement = 
  match statement with
    Block_t(stmts) -> 
      ("{\n" ^ 
        let stmt_lst = string_of_stmt_list f_name stmts in 
            String.concat "" stmt_lst ^ "}\n")
  | Expr_t(expr) -> string_of_expr_t "junk" expr ^ ";\n"
  | Return_t(expr) -> 
      if f_name = "song" then "Write.midi(" ^ string_of_expr_t "junk" expr ^", \"createNotes.mid\");\n" 
    else "return " ^ string_of_expr_t "junk" expr ^ ";\n"
  | If_t(e, s, Block_t([])) -> "if (" ^ string_of_expr_t "junk" e ^ ")\n" ^ string_of_stmt_t f_name s
  | If_t(e, s1, s2) ->  "if (" ^ string_of_expr_t "junk" e ^ ")\n" ^
      string_of_stmt_t f_name s1 ^ "else\n" ^ string_of_stmt_t f_name s2
  | For_t(e1, e2, e3, s) ->
      "for (" ^ string_of_expr_t "junk" e1  ^ " ; " ^ string_of_expr_t "junk" e2 ^ " ; " ^
      string_of_expr_t "junk" e3  ^ ") " ^ string_of_stmt_t f_name s
  | While_t(e, s) -> "while (" ^ string_of_expr_t "junk" e ^ ") " ^ string_of_stmt_t f_name s
  | Vdecl_t(v) -> string_of_vdecl_t v ^ ";\n"
  (* really...the only time that this maters - is here *)
  | Vinit_t(v, e) -> string_of_vdecl_t v ^ " = " ^ string_of_expr_t (string_of_vdecl_name_t v) e ^ "\n" 

 (*| Loop*)

let string_of_fdecl_t fdecl =
  (* no song function has arguments *)
  if fdecl.fname_t = "song" 
    then 
      let stmt_lst = string_of_stmt_list fdecl.fname_t fdecl.body_t in 
        "public static void main(String[] args){\n" ^
          String.concat "" stmt_lst  ^ "}\n}\n"
  else 
    let stmt_lst = string_of_stmt_list fdecl.fname_t fdecl.body_t in
   "private static " ^ (match fdecl.rtype_t with
    Double_t -> "double "
    | Note_t -> "Note "
    | Chord_t -> "CPhrase "
    | Track_t -> "Part "
    | Rest_t -> "Rest "
    | Score_t -> "Score "
    | _ -> "void") ^ 
            fdecl.fname_t ^ "(" ^ String.concat ", " (List.map string_of_vdecl_t fdecl.formals_t) ^ ")\n{\n" ^
                 String.concat "" stmt_lst ^ "}\n"

(*pretty print for program*)
let string_of_program_t (vars, funcs) =
  "\n" ^ imports ^ 
  String.concat "" (List.map string_of_vdecl_t vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl_t funcs)  

(* let finalImports = "\n} \n}" *)


(* look over how were doing the song funcitons... *)



(* WRITE PROGRAM TO FILE  *)
let rec write_to_file file programString =
  let oc = open_out ("java/" ^ file ^ ".java") in 
  fprintf oc "%s" programString;
(*   close_out oc in *)

and string_of_program file (* programString *) (vars, funcs)= 
  (* let globalString = writeGlobalString vars in *)
  let out = sprintf 
    "
    import java.util.*;
    import jm.JMC;
    import jm.music.data.*;
    import jm.util.*; 

    public class %s implements JMC {
      %s

      %s 
    }
      " file (* globalString *) "globals" "functions, etc" (* programString *) in
      write_to_file file out;
      out





