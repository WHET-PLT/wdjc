<<<<<<< HEAD
(* AST *)
=======
type m = Vib | Trem | Bend
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | 
          Geq | Ser | Par | Incr | Decr | Arrow 
>>>>>>> 261833b37c35310313e11b2f72dbbab2441e22b6

(* operation types *)
type op =   Add  | Sub
          | Mult | Div 
          | Ser  | Par 
          | Incr | Decr 
          | Arrow | Vib | Trem | Bend
          | Equal | Neq | Geq | Leq | Greater | Less 


(* Expression type *)
type expr =
    Literal of int
  | Id of string
  | Note of string
  | Rest of string
  | Chord of string
  | Track of string
 (* | Song of string  *)
  | Binop of expr * op * expr
<<<<<<< HEAD
  | Modifier of expr * op
=======
  | Modifier of expr * m 
>>>>>>> 261833b37c35310313e11b2f72dbbab2441e22b6
  | Assign of string * expr
  | Call of string * expr list
  | Array of expr list
  (*an array can be a list of expressions*)
  | Noexpr

<<<<<<< HEAD
  
  (* TODO
=======
    (* TODO
>>>>>>> 261833b37c35310313e11b2f72dbbab2441e22b6
  not sure about 'Modifier'. trying to account for vibrato, tremolo, and bend
  operators. I dont think they can be in binop since these modifiers do not
  require another a sexond expr
   ex. Note a;
       a^;
  *)


(*need to decide if we are keeping loop or not*)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
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
  | Note(n) -> n
  | Rest(r) -> r
  | Chord(c) -> c
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
  (*| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s*)
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