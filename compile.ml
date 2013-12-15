open Sast

let imports =
  "import java.util.*;\n" ^
  "import jm.JMC;\n" ^
  "import jm.music.data.*;\n" ^
  "import jm.util.*;\n\n" ^
  "public class DJ{\n" 
  

(* ------------------------------------------------------------------------------------------------------------- *)

  (* new code based on new ast that emily showed me *)


(*pretty print for expr*)
(*TODO need to decide on arrays*)
let rec string_of_expr_t ?(f_name="null") ?(v_name="null") ex = 
  match ex with
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

  | Assign_t(id, expr) -> string_of_expr_t id ^ " = " ^ string_of_expr_t ~v_name:(string_of_expr_t id) expr (*implementation of optional name param *)

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
      v_name ^ ".add(noteArrayList);\n"

      (*List.map (fun a ->  "noteArrayList.add(" ^ string_of_expr_t a ^ ")\n")  note_list
      name_CPhrase ^ ".add(noteArrayList);" *)

(* What exactly is track.. track creation, because that's what I'm writing it as. also where is the instrument part*)
  | TRACK_CR_t(track_list) ->  
      (* "new Part( \"" ^ string_of_expr_t t ^ "\");"  *)
      " new Part();\n" ^
      "ArrayList<> phraseArrayList = new ArrayList<>();"  (*same as cphrase *)
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
  | Expr_t(expr) -> string_of_expr_t expr ^ ";\n";
  | Return_t(expr) -> 
      if f_name = "Song" then "Write.midi(" ^ string_of_expr_t expr ^", \"midi/createNotes.mid\");\n" 
    else "return " ^ string_of_expr_t expr ^ ";\n";
  | If_t(e, s, Block_t([])) -> "if (" ^ string_of_expr_t e ^ ")\n" ^ string_of_stmt_t f_name s
  | If_t(e, s1, s2) ->  "if (" ^ string_of_expr_t e ^ ")\n" ^
      string_of_stmt_t f_name s1 ^ "else\n" ^ string_of_stmt_t f_name s2
  | For_t(e1, e2, e3, s) ->
      "for (" ^ string_of_expr_t e1  ^ " ; " ^ string_of_expr_t e2 ^ " ; " ^
      string_of_expr_t e3  ^ ") " ^ string_of_stmt_t f_name s
  | While_t(e, s) -> "while (" ^ string_of_expr_t e ^ ") " ^ string_of_stmt_t f_name s
  (* | Assign(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e *)
  | Vdecl_t(v) -> string_of_vdecl_t v ^ ";\n"
  | Vinit_t(v, e) -> string_of_vdecl_t v ^ " = " ^ string_of_expr_t e ^ ";\n"

 (*| Loop*)

let string_of_fdecl_t fdecl =
  (* no song function has arguments *)
  if fdecl.fname_t = "Song" 
    then 
      let stmt_lst = string_of_stmt_list fdecl.fname_t fdecl.body_t in 
        "public static void main(Strings[] args){\n" ^
          String.concat "" stmt_lst  ^ "}\n}\n"
  else 
    let stmt_lst = string_of_stmt_list fdecl.fname_t fdecl.body_t in
   "private static " ^ (match fdecl.rtype_t with
    Int_t -> "int "
    | Note_t -> "Note "
    | Chord_t -> "CPhrase "
    | Track_t -> "Part "
    | Rest_t -> "Rest "
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






