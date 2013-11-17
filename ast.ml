(* AST *)
type m = Vib | Trem | Bend

(* operation types *)
type op =   Add  | Sub
          | Mult | Div 
          | Ser  | Par 
          | Incr | Decr 
          | Arrow
          | Equal | Neq | Geq | Leq | Greater | Less

(* Expression type *)
type expr =
    Literal of int
  | Id of string
  | NOTE_CR of string * string * string * string
  | Rest of string
  | CHORD_CR of string list
  | Track of string
 (* | Song of string  *)
  | Binop of expr * op * expr
  | Modifier of expr * m 
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr
  (* | Array of expr list *)
  (*an array can be a list of expressions*)


  
  (* TODO
  not sure about 'Modifier'. trying to account for vibrato, tremolo, and bend
  operators. I dont think they can be in binop since these modifiers do not
  require another a sexond expr
   ex. Note a;
       a^;
  *)

(*  Note type  *)
(* type note = {
    pitch : int;
    volume : int;
    instr : int;
    dur : int;
  } *)

(*need to decide if we are keeping loop or not*)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Loop of expr * expr * stmt

(* funciton declaration *)
type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

(*ast is a list of stmts and list of function dels*)
type program = string list * func_decl list

(*pretty print for expr*)
(*TODO need to decide on arrays*)
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | NOTE_CR(a, b, c, d) ->
      "(" ^ a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d ^ ")"
  | Rest(r) -> r
  | CHORD_CR(note_list) -> 
      "(" ^ String.concat " : " note_list ^ ")"
  | Track(t) -> t
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	    Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | Ser -> "." | Par -> ":" | Incr -> "++" | Decr -> "--"
      | Arrow -> "->") ^ " " ^
      string_of_expr e2
  (*again, not sure about this section*)
  | Modifier(e1, m) ->
      string_of_expr e1 ^
      (match m with
      Vib -> "^" | Trem -> "~" | Bend -> "%")
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
(*| Array*) 


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
 (* | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e ^ " ; "*)
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
 (*| Loop*)

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

(*pretty print for program*)
let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)  

(* let string_of_note_cr (a, b, c, d) = 
   "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ 
    ", " ^ string_of_int c ^ ", " ^ string_of_int d")\n"


 *)
