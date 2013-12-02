(* AST *)
type modif = Vib | Trem | Bend | Incr | Decr

(* Not sure if I should make this a string *)
type note_attribute = Pitch | Vol | Dur | Instr

(*our data types*)
type dType = Int | Note | Chord | Track | Rest 

(* operation types *)
type op =   Add  | Sub
          | Mult | Div 
          | Ser  | Par  
          | Arrow
          | Equal | Neq | Geq | Leq | Greater | Less

(* Expression type *)
type expr =
    Literal of int
  | ACCESSOR of string * note_attribute
  | Id of string
  | NOTE_CR of string * string * string * string
  | REST_CR of int
  | CHORD_CR of string list
  | Track of string
  | Binop of expr * op * expr
  | Modifier of expr * modif 
  | Assign of expr * expr
  | Call of string * expr list
  | Noexpr
 
  (* | Array of expr list *)
  (*an array can be a list of expressions*)

(*variable declaration*)
type var_decl = {
  vType : dType;
  vName : string;
}

type var_init = {
  vDecl : var_decl;
  vExpr : string;
}

(*need to decide if we are keeping loop or not*)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  (* | Assign of var_decl * expr *)
  | Vdecl of var_decl
  | Vinit of var_decl * expr  
 (* | Loop of expr * expr * stmt *)


(* funciton declaration *)
type func_decl = {
    rtype : dType;
    fname : string;
    formals : var_decl list;
    body : stmt list;
  }

(*ast is a list of variables and list of function dels*)
type program = var_decl list * func_decl list

(*pretty print for expr*)
(*TODO need to decide on arrays*)

let string_of_vdecl v = 
  (match v.vType with
    Int -> "int "
    | Note -> "note "
    | Chord -> "chord "
    | Track -> "track "
    | Rest -> "rest ") ^ v.vName
  
  let string_of_vinit v = 
    string_of_vdecl v.vDecl ^ " = " ^ string_of_expr expr

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | NOTE_CR(a, b, c, d) ->
      "(" ^ a ^ ", " ^ b ^ ", " ^ c ^ ", " ^ d ^ ")"
  | REST_CR(r) -> "(" ^ string_of_int r ^ ")" (* should this really be string of literal or something? *)
  | ACCESSOR(a, b) -> 
      a ^ " -> " ^ (
      match b with
        Pitch -> "pitch" | Vol -> "vol" | Instr -> "instr" | Dur -> "dur"
      )
  | Assign(id, expr) -> id ^ " = " ^ string_of_expr expr
  | CHORD_CR(note_list) -> 
      "(" ^ String.concat " : " note_list ^ ")"
  | Track(t) -> t
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
  | Vdecl(v) -> string_of_vdecl v ^ ";"
 (*| Loop*)


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

