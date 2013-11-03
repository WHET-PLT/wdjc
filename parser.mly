%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE ASSIGN
%token <int> LITERAL
%token <string> ID
%token EOF

%left PLUS MINUS
%left TIMES DIVIDE

%%

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }