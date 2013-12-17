open Sast
open Printf

let num_tracks = ref 0;;
let imports = "import java.util.*;\n" ^
              "import jm.JMC;\n" ^
              "import jm.music.data.*;\n" ^
              "import jm.util.*;\n\n" ^
              "public class DJ implements JMC{\n" 
  

(* ------------------------------------------------------------------------------------------------------------- *)

  (* new code based on new ast that emily showed me *)

(* WRITE PROGRAM TO FILE  *)
let rec write_to_file programString =
  let oc = open_out ("java/src/main.java") in 
  fprintf oc "%s" programString;
(*   close_out oc in *)

and string_of_program file (vars, funcs)= 
  let global_string = write_global_string vars in
    let func_string = write_func_string file funcs in 
  let out = sprintf 
    "
    import java.util.*;
    import jm.JMC;
    import jm.music.data.*;
    import jm.util.*; 

    public class main implements JMC {
      %s

      %s 
    }
      " global_string func_string in
      write_to_file out;
      out
      
and write_global_string vars =
  let gs = parse_global vars in
  if List.length gs >= 1
    then sprintf "%s" ((String.concat ";\n" gs) ^ ";\n")
  else
    sprintf "%s" (String.concat ";\n" gs)

and write_func_string file funcs =
  let fs = parse_funcs file funcs in
    sprintf "%s" (String.concat "" fs)

and parse_global = function
  [] -> []
  | h::t -> let global_string = (write_vdecl h) in global_string::(parse_global t)

and parse_funcs file = function
  [] -> []
  | h::t -> let funcs_string = (write_fdecl file h) in funcs_string::(parse_funcs file t)

and write_vdecl v = 
  (match v.vType_t with
    Double_t -> "double "
    | Note_t -> "Note "
    | Chord_t -> "CPhrase "
    | Track_t -> "Part "
    | Rest_t-> "Rest "
    | Score_t -> "Score ") ^ v.vName_t

and write_vdecl_name v =  v.vName_t

and write_fdecl file f =
  (* no song function has arguments *)
  let stmt_list = write_stmt_list file f.fname_t f.body_t in
    let stmt_string = String.concat "" stmt_list in 
  (* SONG FUNCTION *)
  if f.fname_t = "song" 
    then 
        "public static void main(String[] args){\n" ^
        stmt_string ^
        "\n}\n"
  (* NON-SONG FUNCTION *)
  else 
      let formals_list = List.map string_of_vdecl_t f.formals_t in
        let formals_str = String.concat ", " formals_list in
          "private static " ^ 
            (match f.rtype_t with
              Double_t -> "double "
              | Note_t -> "Note "
              | Chord_t -> "CPhrase "
              | Track_t -> "Part "
              | Rest_t -> "Rest "
              | Score_t -> "Score "
              | _ -> "void") ^ 
            f.fname_t ^ "( " ^ formals_str ^ " )" ^
            "\n{" ^ stmt_string ^ "\n}\n"

and write_stmt_list file fname = function 
  [] -> []
  | h::t -> let string_stmt_list = ((write_stmt file fname h)) in string_stmt_list::(write_stmt_list file fname t)

and write_stmt file f_name statement = 
  match statement with
    Block_t(stmts) -> sprintf "%s" (write_stmt_block file f_name stmts)
  | Expr_t(expr) -> sprintf "%s" (write_expr f_name expr)
  | Return_t(expr) -> 
    let ex1 = write_expr "junk" expr in
      if f_name = "song" then 
        sprintf "%s" "Write.midi(" ^ ex1 ^", \"" ^ file ^ "\");\n" 
      else sprintf "%s" "return " ^ ex1 ^ ";\n"
  | If_t(e, s, Block_t([])) -> 
      let ex1 = write_expr "junk" e in 
        sprintf "%s" "if (" ^ ex1 ^ ")\n" ^ write_stmt file f_name s
  | If_t(e, s1, s2) ->  
      let ex1 = write_expr "junk" e in
        let s1 = write_stmt file f_name s1 in
          let s2 = write_stmt file f_name s2 in
            sprintf "%s" "if (" ^ ex1 ^ ")\n" ^ s1 ^ "else\n" ^ s2
  | For_t(e1, e2, e3, s) -> 
    let ex1 = write_expr "junk" e1 in
      let ex2 = write_expr "junk" e2 in
        let ex3 = write_expr "junk" e3 in
          let s1 = write_stmt file f_name s in
            sprintf "%s" "for ( (int)" ^ ex1  ^ "" ^ ex2 ^ " ; " ^ ex3 ^ ") " ^ s1
  | While_t(e, s) -> 
  let ex1 = write_expr "junk" e in
    let s1 = write_stmt file f_name s in
      sprintf "%s" ("while (" ^ ex1^ ") " ^ s1)
  | Vdecl_t(v) -> sprintf "%s" (write_vdecl v ^ ";\n")
  | Vinit_t(v, e) -> 
    let var = write_vdecl v in 
      (* let name = write_expr "junk" v  *)
      let ex1 = (write_expr v.vName_t e) in 
        sprintf "%s" (var ^ " = " ^ ex1 ^ ";\n")

and write_stmt_block file f_name stmts = 
  let stmt_list = (write_stmt_list file f_name stmts) in
    let stmt_string = String.concat "" stmt_list in 
      "{\n" ^ stmt_string ^ "}\n"

and write_expr v_name ex = 
  match ex with
    Literal_t(l) -> sprintf "%s" l
  | Id_t(s) -> sprintf "%s" s
  | NOTE_CR_t(a, b, c) -> 
      let ex1 = write_expr "junk" a in
        let ex2 = write_expr "junk" b in
          let ex3 = write_expr "junk" c in 
     sprintf "%s" "new Note((double)" ^ ex1 ^ ", " ^  ex2 ^ ", (int) " ^ ex3 ^ ")"
  | REST_CR_t(r) -> 
      let ex1  = write_expr "junk" r in
      sprintf "%s" "new Note(( REST, " ^ ex1 ^ ")" 
  | ACCESSOR_t(a, b) -> 
      let ex1 = write_expr "junk" a in
        let access_type = (
          match b with
          Pitch_t -> "getFrequency()" | Vol_t -> "getDynamic()" |  Dur_t -> "getDuration()"
          ) in
      sprintf "%s" ex1 ^ "." ^ access_type
  | Assign_t(id, expr) -> 
      let identifier = write_expr "junk" id in
        let ex = write_expr identifier expr in
          sprintf "%s" identifier ^ " = " ^ ex ^ ";\n"
  | CHORD_CR_t(note_list) -> 
    let notes = write_expr_list "junk" note_list in
      let notes_string = String.concat ", " notes in
        sprintf "%s" " new CPhrase();\n" ^ "Note [] notes_array = {" ^ notes_string ^ "};\n" ^  v_name^ ".addChord(notes_array)"
(* What exactly is track.. track creation, because that's what I'm writing it as. also where is the instrument part*)
  | TRACK_CR_t(instr) ->
    let ex1  = write_expr "junk" instr in
      sprintf "%s" "new Part(" ^ ex1 ^ ");"
  (* GLOBAL VARIABLES???? *)
  | SCORE_CR_t(track_list) ->
    let track_adds = write_score_track_list v_name track_list in
      let track_str = String.concat ";\n" track_adds in
        sprintf "%s" ("new Score();\n" ^ track_str)
  | Binop_t(e1, o, e2) ->
      let ex1 = write_expr "junk" e1 in
        let ex2 = write_expr "junk" e2 in
          let op = (
            match o with
              Add_t -> "+" | Sub_t -> "-" | Mult_t -> "*" | Div_t -> "/"
              | Equal_t -> "==" | Neq_t -> "!="
              | Less_t -> "<" | Leq_t -> "<=" | Greater_t -> ">" | Geq_t -> ">=" 
              | Ser_t -> "" | Par_t -> "" ) in
      sprintf "%s" (ex1 ^ " " ^ op ^ " " ^ ex2)
  | Modifier_t(e1, modif) ->
    let ex1 = write_expr "junk" e1 in
      let modifier = (
        match modif with
        Vib_t -> " " |
        Trem_t -> " " | 
        Incr_t -> ".setPitch((" ^ write_expr "junk" e1 ^".getPitch()) + 50)"  | 
        Decr_t -> ".setPitch((" ^ write_expr "junk" e1 ^".getPitch())  -50)") in
      sprintf "%s" (ex1 ^ modifier)
  | Call_t(f, el) ->
        let calls = write_expr_list "junk" el in
        sprintf "%s" (f ^ "(" ^ String.concat ", " calls ^ ")")
  | Noexpr_t -> sprintf "%s" ""

and write_score_track_list vname = function 
  [] -> []
  | h::t -> let track_str_list = (vname ^ ".addPart(" ^ (write_expr "junk" h) ^ ")") in track_str_list::(write_score_track_list vname t)

and write_chord_list vname chord_list = 
  match chord_list with 
    [] -> []
  | h::t -> let track_str_list = (vname ^ ".addCPhrase(" ^ (write_expr "junk" h) ^ ")") in track_str_list::(write_score_track_list vname t)
  
  
and write_expr_list v_name expr_list = 
  match expr_list with
  [] -> []
  | hd::tl -> let string_expr_list = (write_expr v_name hd) in string_expr_list::(write_expr_list v_name tl) 




