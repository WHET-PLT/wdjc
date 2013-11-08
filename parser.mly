%{ open Ast %}

/*token section not yet finished. need to check if order matters*/
%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE COMMA SEMI
%token PLUS MINUS TIMES DIVIDE ASSIGN 
%token SERIAL PARALLEL
%token VIB TREM BEND ARROW
%token EQ NEQ INCR DECR
%token LT LEQ GT GEQ
%token IF ELSE FOR LOOP RETURN INT
%token FUN VOL DUR
%token <int> LITERAL
%token <string> ID
%token <string> NOTE
%token <string> REST
%token <string> CHORD
%token <string> SONG
%token EOF

/*started associativity. need further clarification.*/
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left SERIAL PARALLEL
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }