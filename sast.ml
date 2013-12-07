(* SAST *)
type modif_t = Vib | Trem | Bend | Incr | Decr

(* Not sure if I should make this a string *)
type note_attribute_t = Pitch | Vol | Dur | Instr

type dType_t = Int | Note | Chord | Track | Rest 

(* operation types *)
type op_t =   Add  | Sub
          | Mult | Div 
          | Ser  | Par  
          | Arrow
          | Equal | Neq | Geq | Leq | Greater | Less

(* Expression type *)
(* Expression type *)
type expr_t =
    Literal of int
  | Id of string
  | NOTE_CR of string * string * string
  | Rest of string
  | CHORD_CR of string list
  | TRACK_CR of string
  | ACCESSOR of string * note_attribute_t
  | Binop of expr_t * op_t * expr_t
  | Modifier of expr_t * modif_t 
  | Assign of string * expr_t
  | Call of string * expr_t list
  | Noexpr
 
  (* | Array of expr list *)
  (*an array can be a list of expressions*)

(*variable declaration*)
type var_decl_t = {
  vType : dType_t;
  vName : string;
}

type var_init = {
  vDecl : var_decl_t;
  vExpr : expr_t;
}

(*need to decide if we are keeping loop or not*)
type stmt_t =
    Block of stmt_t list
  | Expr of expr_t
  | Return of expr_t
  | If of expr_t * stmt_t * stmt_t
  | For of expr_t * expr_t * expr_t * stmt_t
  | While of expr_t * stmt_t
  | Vdecl of var_decl_t
  | Vinit of var_decl_t * expr_t


(* funciton declaration *)
type func_decl_t = {
    rtype : dType_t;
    fname : string;
    formals : var_decl_t list;
    body : stmt_t list;
  }

(*ast is a list of variables and list of function dels*)
type program_t = var_decl_t list * func_decl_t list


let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | NOTE_CR(a, b, c) ->
      "(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ", " ^ string_of_expr c ^ ")"
  | REST_CR(r) -> "(" ^ string_of_expr r ^ ")" 
  | TRACK_CR(t) -> "(" ^ string_of_expr t ^ ")" 
  | ACCESSOR(a, b) -> 
      a ^ " -> " ^ (
      match b with
        Pitch -> "pitch" | Vol -> "vol" | Instr -> "instr" | Dur -> "dur"
      )
  | Assign(id, expr) -> string_of_expr id ^ " = " ^ string_of_expr expr
  | CHORD_CR(note_list) -> 
      "(" ^ String.concat " : " note_list ^ ")"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
      Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | Ser -> "." | Par -> ":" | Arrow -> "->") ^ " " ^
      string_of_expr e2
  | Modifier(e1, modif) ->
      string_of_expr e1 ^
      (match modif with
      Vib -> "^" | Trem -> "~" | Bend -> "%" | Incr -> "++" | Decr -> "--")
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""


let string_of_vdecl v = 
  (match v.vType with
    Int -> "int "
    | Note -> "note "
    | Chord -> "chord "
    | Track -> "track "
    | Rest -> "rest ") ^ v.vName


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
  | Vdecl(v) -> string_of_vdecl v ^ ";\n"
  | Vinit(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e ^ ";\n"


let string_of_fdecl fdecl =
   (match fdecl.rtype with
    Int -> "int "
    | Note -> "note "
    | Chord -> "chord "
    | Track -> "track "
    | Rest -> "rest ") ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

(*pretty print for program*)
let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)  



