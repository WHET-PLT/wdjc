%{ open Ast %}

/*token section not yet finished. need to check if order matters*/
/*As of 11-09-13 we dont think theres precedence here */
%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE 
%token COMMA SEMI ASSIGN
%token PLUS MINUS TIMES DIVIDE 
%token SERIAL PARALLEL
%token VIB TREM BEND ARROW
%token EQ NEQ INCR DECR
%token LT LEQ GT GEQ
%token IF ELSE FOR WHILE LOOP RETURN INT
%token FUN VOL DUR PIT INSTR
%token <int> LITERAL
%token <string> ID

/*
  I think they are strings just b/c not sure
  what else we could make them. 
  Still unsure about SONG though, do we need to include
  it? Do we use SONG anywhere else but as a microc like 'main'
  function. If so, I do not think we need to include it in the
  scanner, parser, ast. microc doesnt have main in its files
  i think. need to check on that. Also, lets clear up arrays on
  wed. - Tom
*/
/*
- that makes sense, then is it just a list or something in the ast? like "program"?
- but does microc depend on main? I thought it was just a script, top to bottom? maybe?
- if we dont have it in the scanner, is it just read as a function ID?
WFW*/

/*
- I am not completely sure about it being a list. All of those are good questions. We may
need to look further into microc's compile file for the answer. I think the answer might
have something to do with checking for keywords in a list.
*/

%token NOTE REST CHORD TRACK
%token EOF

/*started associativity. need further clarification.*/
/*precedence is ordered bottom to top */
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
/* arrow - symbol: -> 
VIB - vibrato (^)
TREM - tremolo (~)
BEND - pitch bend (%)
*/
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



assignment_opt: 
  /* nothing */ {[] }

fdecl:
  ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { { fname = $1;
	formals = $3;
	locals = List.rev $6;
	body = List.rev $7 } }


/* FORMALS - 
optional function arguments 
*/
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

/* recursive list of function arguments */
formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }


/* VARIABLE DECLARATIONS */
vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

/* adds to local list in ast */
vdecl:
  INT ID SEMI { $2 }
  | NOTE ID SEMI { $2 }
  | CHORD ID SEMI { $2 }
  | TRACK ID SEMI { $2 }

/* ASSIGNMENT 
vinit:
  INT ID ASSIGN expr SEMI { Assign($2, $4) }
*/
/* STATEMENTS */
stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

/*
  stmt section not finished.
  TODO: decide on whether or not we are using 'loop'
*/
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

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

/*
  expr section not finished.
  TODO: need clarification on 'Modifier' section: VIB, TREM, BEND.
  need clarification on INCR, DECR, ARROW
*/
expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
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
  | expr INCR   expr { Binop($1, Incr,   $3) }
  | expr DECR   expr { Binop($1, Decr,   $3) }
  | expr ARROW  expr { Binop($1, Arrow,   $3) }
  /* | ID ASSIGN expr   { Assign($1, $3)} */
  | expr SERIAL expr { Binop($1, Ser, $3) }
  | expr PARALLEL expr { Binop ($1, Par, $3) }
  | expr VIB         { Modifier($1, Vib) }
  | expr TREM        { Modifier($1, Trem) }
  | expr BEND        { Modifier($1, Bend) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  /*| LBRACKET actuals_opt RBRACKET { Array($?) } */

 /* actuals - 
 When you call the function you use actuals_opt
 */
  actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

  actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

