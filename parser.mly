%{ open Ast %}

%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE 
%token COMMA SEMI ASSIGN
%token PLUS MINUS TIMES DIVIDE 
%token SERIAL PARALLEL
%token VIB TREM BEND ARROW
%token EQ NEQ INCR DECR
%token LT LEQ GT GEQ
%token IF ELSE FOR WHILE LOOP RETURN INT
%token FUN VOL DUR PITCH INSTR
%token <int> LITERAL
%token <string> ID
%token NOTE REST CHORD TRACK
%token EOF


/*ie TIMES DIVIDE is higher precedence than ASSIGN*/
%nonassoc NOELSE
%nonassoc ELSE
/*Right associative because if you have a = b = c you want
to do (a = (b = c))*/
%right ASSIGN
/* Equals/neq association: (a == b) == c */
%left EQ NEQ
%left LT GT LEQ GEQ
/*SERIAL/PARALLEL defaulted to PLUS/MINUS Associativity*/
%left SERIAL PARALLEL
%left PLUS MINUS
%left TIMES DIVIDE
%left VIB TREM BEND ARROW
/*incr - incrememnt (++); decr - decrement  (--) */
/*Ex: (note++)++ */
%left INCR DECR



%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

/*  --- FUNCTION --- */
fdecl:
  ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { { fname = $1;
	formals = $3;
	locals = List.rev $6;
	body = List.rev $7 } }

/* --- FORMALS --- */
/* optional function arguments */
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }


/* --- VARIABLE DECLARATIONS --- */
vdecl:
  INT ID SEMI { $2 }
  | NOTE ID SEMI { $2 }
  | CHORD ID SEMI { $2 }
  | TRACK ID SEMI { $2 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

/*  --- NOTE  --- */
note_cr:
  LPAREN ID COMMA ID COMMA ID COMMA ID RPAREN
    { NOTE_CR($2, $4, $6, $8) }

/* --- CHORD --- */
chord_cr:
    LPAREN RPAREN { CHORD_CR ([]) }
    | LPAREN chord_list RPAREN { CHORD_CR ( List.rev $2 ) }

chord_list:
    ID { [$1] }
    | chord_list PARALLEL ID { $3 :: $1 }

/* --- ACCESSOR --- */
accessor:
  ID ARROW note_attribute { ACCESSOR($1, $3) }

/* List of note attributes */
note_attribute:
  PITCH {Pitch}
  | VOL {Vol}
  | DUR {Dur}
  | INSTR {Instr}

/* --- MODIFIERS --- */
/*
modifier:

modifier_options:
  BEND    {$1}
  | VIB   {$1}
  | TREM  {$1}

*/
/* --- STATEMENTS --- */

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  /*| LOOP LPAREN expr RPAREN stmt { Loop($3, $5) }*/

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

/* --- EXPRESSIONS --- */

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | chord_cr         { $1 }
  | note_cr          { $1 }
  | accessor         { $1 }
  /*| modifier         { $1 }*/
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr ARROW  expr { Binop($1, Arrow,   $3) }
  | ID ASSIGN expr   { Assign($1, $3)} 
  | ID ASSIGN   expr { Assign($1, $3)}
  | expr SERIAL expr { Binop($1, Ser, $3) }
  | expr PARALLEL expr { Binop ($1, Par, $3) }
  | expr INCR        { Modifier($1, Incr) }
  | expr DECR        { Modifier($1, Decr) }
  | expr VIB         { Modifier($1, Vib) }
  | expr TREM        { Modifier($1, Trem) }
  | expr BEND        { Modifier($1, Bend) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  /*| LBRACKET actuals_opt RBRACKET { Array($?) } */

 /* actuals - When you call the function you use actuals_opt?? */
  actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

  actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

