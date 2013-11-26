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
  | ACCESSOR of string * note_attribute_t
  | Id of string
  | NOTE_CR of string * string * string * string
  | Rest of string
  | CHORD_CR of string list
  | Track of string
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

(*need to decide if we are keeping loop or not*)
type stmt_t =
    Block of stmt_t list
  | Expr of expr_t
  | Return of expr_t
  | If of expr_t * stmt_t * stmt_t
  | For of expr_t * expr_t * expr_t * stmt_t
  | While of expr_t * stmt_t
  (*| Assign of var_decl * expr
  | Vdecl of var_decl*)
 (* | Loop of expr * expr * stmt *)


(* funciton declaration *)
type func_decl_t = {
    rtype : dType_t;
    fname : string;
    formals : var_decl_t list;
    locals : var_decl_t list;
    body : stmt_t list;
  }

(*ast is a list of variables and list of function dels*)
type program_t = var_decl_t list * func_decl_t list


