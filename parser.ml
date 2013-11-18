type token =
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | SEMI
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | SERIAL
  | PARALLEL
  | VIB
  | TREM
  | BEND
  | ARROW
  | EQ
  | NEQ
  | INCR
  | DECR
  | LT
  | LEQ
  | GT
  | GEQ
  | IF
  | ELSE
  | FOR
  | WHILE
  | LOOP
  | RETURN
  | INT
  | FUN
  | VOL
  | DUR
  | PITCH
  | INSTR
  | LITERAL of (int)
  | ID of (string)
  | NOTE
  | REST
  | CHORD
  | TRACK
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 54 "parser.ml"
let yytransl_const = [|
  257 (* LBRACK *);
  258 (* RBRACK *);
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* LBRACE *);
  262 (* RBRACE *);
  263 (* COMMA *);
  264 (* SEMI *);
  265 (* ASSIGN *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* TIMES *);
  269 (* DIVIDE *);
  270 (* SERIAL *);
  271 (* PARALLEL *);
  272 (* VIB *);
  273 (* TREM *);
  274 (* BEND *);
  275 (* ARROW *);
  276 (* EQ *);
  277 (* NEQ *);
  278 (* INCR *);
  279 (* DECR *);
  280 (* LT *);
  281 (* LEQ *);
  282 (* GT *);
  283 (* GEQ *);
  284 (* IF *);
  285 (* ELSE *);
  286 (* FOR *);
  287 (* WHILE *);
  288 (* LOOP *);
  289 (* RETURN *);
  290 (* INT *);
  291 (* FUN *);
  292 (* VOL *);
  293 (* DUR *);
  294 (* PITCH *);
  295 (* INSTR *);
  298 (* NOTE *);
  299 (* REST *);
  300 (* CHORD *);
  301 (* TRACK *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  296 (* LITERAL *);
  297 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\002\000\002\000\002\000\002\000\005\000\005\000\008\000\009\000\
\009\000\010\000\010\000\011\000\012\000\012\000\012\000\012\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\006\000\
\006\000\015\000\015\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\016\000\016\000\
\017\000\017\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\000\000\002\000\009\000\002\000\
\003\000\001\000\003\000\003\000\001\000\001\000\001\000\001\000\
\002\000\003\000\003\000\005\000\007\000\009\000\005\000\000\000\
\002\000\000\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\002\000\004\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\003\000\000\000\000\000\000\000\000\000\000\000\009\000\
\007\000\000\000\000\000\010\000\011\000\012\000\000\000\000\000\
\013\000\008\000\000\000\014\000\000\000\000\000\032\000\004\000\
\000\000\000\000\000\000\000\000\036\000\000\000\039\000\038\000\
\040\000\033\000\000\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\058\000\059\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\000\000\062\000\027\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
\022\000\023\000\021\000\024\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\051\000\052\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\000\000\061\000\000\000\000\000\000\000\000\000\031\000\000\000\
\000\000\000\000\000\000\000\000\029\000\000\000\000\000\000\000\
\015\000\030\000"

let yydgoto = "\002\000\
\003\000\009\000\010\000\018\000\027\000\029\000\019\000\039\000\
\040\000\046\000\041\000\093\000\042\000\043\000\082\000\086\000\
\087\000"

let yysindex = "\001\000\
\000\000\000\000\005\001\223\254\013\255\250\254\010\255\016\255\
\000\000\000\000\055\255\024\255\060\255\062\255\078\255\000\000\
\000\000\086\255\089\255\000\000\000\000\000\000\095\255\067\255\
\000\000\000\000\009\001\000\000\043\255\014\255\000\000\000\000\
\108\255\116\255\118\255\019\255\000\000\109\255\000\000\000\000\
\000\000\000\000\179\000\000\000\096\255\255\254\151\255\047\255\
\019\255\019\255\019\255\199\000\019\255\019\255\120\255\000\000\
\019\255\019\255\019\255\019\255\019\255\019\255\000\000\000\000\
\000\000\019\255\019\255\019\255\019\255\019\255\019\255\019\255\
\019\255\019\255\083\255\000\000\088\255\000\000\000\000\175\255\
\217\000\122\255\199\255\000\000\217\000\132\255\139\255\217\000\
\000\000\000\000\000\000\000\000\000\000\019\001\019\001\039\001\
\039\001\011\001\011\001\011\255\235\000\235\000\000\000\000\000\
\253\000\253\000\253\000\253\000\153\255\000\000\076\255\019\255\
\076\255\000\000\019\255\140\255\154\255\172\255\000\000\217\000\
\177\255\076\255\019\255\141\255\000\000\200\255\201\255\076\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\206\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\203\255\000\000\000\000\000\000\000\000\
\000\000\000\000\204\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\061\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\127\255\000\000\000\000\
\000\000\000\000\000\000\000\000\241\255\000\000\000\000\000\000\
\000\000\220\255\000\000\000\000\225\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\253\254\000\000\000\000\000\000\054\255\000\000\228\255\003\255\
\000\000\000\000\000\000\000\000\000\000\057\000\081\000\009\000\
\033\000\096\000\111\000\223\255\005\255\106\255\000\000\000\000\
\120\000\135\000\144\000\159\000\000\000\000\000\000\000\220\255\
\000\000\000\000\000\000\000\000\092\255\000\000\000\000\065\255\
\000\000\000\000\242\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\233\000\000\000\000\000\000\000\239\000\000\000\000\000\
\000\000\000\000\000\000\000\000\190\255\226\255\148\255\000\000\
\000\000"

let yytablesize = 574
let yytable = "\047\000\
\035\000\001\000\076\000\118\000\035\000\052\000\054\000\011\000\
\045\000\054\000\054\000\045\000\045\000\077\000\126\000\012\000\
\030\000\044\000\080\000\081\000\083\000\030\000\085\000\088\000\
\045\000\045\000\094\000\095\000\096\000\097\000\098\000\099\000\
\069\000\070\000\013\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\107\000\108\000\117\000\030\000\119\000\031\000\
\032\000\030\000\014\000\031\000\079\000\037\000\045\000\125\000\
\015\000\065\000\037\000\038\000\065\000\130\000\016\000\032\000\
\017\000\032\000\032\000\020\000\066\000\021\000\033\000\066\000\
\034\000\035\000\033\000\036\000\034\000\035\000\030\000\036\000\
\031\000\081\000\037\000\038\000\120\000\022\000\037\000\038\000\
\032\000\023\000\032\000\032\000\081\000\032\000\028\000\024\000\
\028\000\028\000\053\000\025\000\032\000\032\000\075\000\033\000\
\054\000\034\000\035\000\026\000\036\000\046\000\049\000\053\000\
\046\000\046\000\055\000\037\000\038\000\054\000\050\000\028\000\
\051\000\028\000\028\000\109\000\028\000\046\000\046\000\055\000\
\110\000\112\000\037\000\028\000\028\000\037\000\037\000\114\000\
\037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\115\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\078\000\089\000\090\000\091\000\092\000\116\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\111\000\123\000\121\000\127\000\122\000\124\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\113\000\128\000\129\000\067\000\005\000\006\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\053\000\034\000\063\000\053\000\053\000\064\000\
\053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
\053\000\053\000\053\000\053\000\018\000\034\000\053\000\053\000\
\053\000\053\000\037\000\037\000\037\000\037\000\037\000\018\000\
\037\000\037\000\037\000\028\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\043\000\048\000\000\000\043\000\
\043\000\000\000\043\000\043\000\043\000\043\000\043\000\043\000\
\000\000\000\000\000\000\000\000\043\000\043\000\000\000\000\000\
\043\000\043\000\043\000\043\000\044\000\000\000\000\000\044\000\
\044\000\000\000\044\000\044\000\044\000\044\000\044\000\044\000\
\000\000\000\000\000\000\000\000\044\000\044\000\000\000\000\000\
\044\000\044\000\044\000\044\000\041\000\000\000\000\000\041\000\
\041\000\000\000\041\000\041\000\000\000\000\000\041\000\041\000\
\000\000\000\000\000\000\000\000\041\000\041\000\000\000\000\000\
\041\000\041\000\041\000\041\000\042\000\000\000\000\000\042\000\
\042\000\000\000\042\000\042\000\000\000\000\000\042\000\042\000\
\000\000\000\000\000\000\056\000\042\000\042\000\056\000\056\000\
\042\000\042\000\042\000\042\000\000\000\056\000\056\000\000\000\
\000\000\000\000\057\000\056\000\056\000\057\000\057\000\056\000\
\056\000\056\000\056\000\047\000\057\000\057\000\047\000\047\000\
\000\000\000\000\057\000\057\000\000\000\000\000\057\000\057\000\
\057\000\057\000\048\000\047\000\047\000\048\000\048\000\047\000\
\047\000\047\000\047\000\049\000\000\000\000\000\049\000\049\000\
\000\000\000\000\048\000\048\000\000\000\000\000\048\000\048\000\
\048\000\048\000\050\000\049\000\049\000\050\000\050\000\049\000\
\049\000\049\000\049\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\050\000\050\000\000\000\000\000\050\000\050\000\
\050\000\050\000\056\000\000\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\072\000\073\000\074\000\084\000\000\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\073\000\074\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\000\000\000\000\
\069\000\070\000\071\000\072\000\073\000\074\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\000\000\000\000\069\000\070\000\057\000\058\000\059\000\060\000\
\000\000\000\000\063\000\064\000\065\000\066\000\059\000\060\000\
\069\000\070\000\063\000\064\000\065\000\066\000\004\000\000\000\
\069\000\070\000\004\000\000\000\000\000\005\000\006\000\000\000\
\007\000\008\000\006\000\000\000\007\000\008\000\063\000\064\000\
\065\000\066\000\000\000\000\000\069\000\070\000"

let yycheck = "\030\000\
\004\001\001\000\004\001\112\000\008\001\036\000\004\001\041\001\
\004\001\007\001\008\001\007\001\008\001\015\001\123\000\003\001\
\003\001\004\001\049\000\050\000\051\000\003\001\053\000\054\000\
\020\001\021\001\057\000\058\000\059\000\060\000\061\000\062\000\
\022\001\023\001\041\001\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\073\000\074\000\111\000\003\001\113\000\005\001\
\006\001\003\001\041\001\005\001\006\001\040\001\041\001\122\000\
\041\001\004\001\040\001\041\001\007\001\128\000\008\001\003\001\
\041\001\005\001\006\001\008\001\004\001\008\001\028\001\007\001\
\030\001\031\001\028\001\033\001\030\001\031\001\003\001\033\001\
\005\001\112\000\040\001\041\001\115\000\008\001\040\001\041\001\
\028\001\004\001\030\001\031\001\123\000\033\001\003\001\007\001\
\005\001\006\001\003\001\005\001\040\001\041\001\007\001\028\001\
\009\001\030\001\031\001\041\001\033\001\004\001\003\001\003\001\
\007\001\008\001\019\001\040\001\041\001\009\001\003\001\028\001\
\003\001\030\001\031\001\041\001\033\001\020\001\021\001\019\001\
\041\001\008\001\004\001\040\001\041\001\007\001\008\001\004\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\007\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\004\001\036\001\037\001\038\001\039\001\007\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\004\001\008\001\041\001\041\001\029\001\007\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\004\001\004\001\004\001\000\000\004\001\004\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\004\001\008\001\004\001\007\001\008\001\004\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\004\001\004\001\024\001\025\001\
\026\001\027\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\027\000\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\004\001\031\000\255\255\007\001\
\008\001\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\020\001\021\001\255\255\255\255\
\024\001\025\001\026\001\027\001\004\001\255\255\255\255\007\001\
\008\001\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\020\001\021\001\255\255\255\255\
\024\001\025\001\026\001\027\001\004\001\255\255\255\255\007\001\
\008\001\255\255\010\001\011\001\255\255\255\255\014\001\015\001\
\255\255\255\255\255\255\255\255\020\001\021\001\255\255\255\255\
\024\001\025\001\026\001\027\001\004\001\255\255\255\255\007\001\
\008\001\255\255\010\001\011\001\255\255\255\255\014\001\015\001\
\255\255\255\255\255\255\004\001\020\001\021\001\007\001\008\001\
\024\001\025\001\026\001\027\001\255\255\014\001\015\001\255\255\
\255\255\255\255\004\001\020\001\021\001\007\001\008\001\024\001\
\025\001\026\001\027\001\004\001\014\001\015\001\007\001\008\001\
\255\255\255\255\020\001\021\001\255\255\255\255\024\001\025\001\
\026\001\027\001\004\001\020\001\021\001\007\001\008\001\024\001\
\025\001\026\001\027\001\004\001\255\255\255\255\007\001\008\001\
\255\255\255\255\020\001\021\001\255\255\255\255\024\001\025\001\
\026\001\027\001\004\001\020\001\021\001\007\001\008\001\024\001\
\025\001\026\001\027\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\020\001\021\001\255\255\255\255\024\001\025\001\
\026\001\027\001\008\001\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\008\001\255\255\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\255\255\255\255\022\001\023\001\010\001\011\001\012\001\013\001\
\255\255\255\255\016\001\017\001\018\001\019\001\012\001\013\001\
\022\001\023\001\016\001\017\001\018\001\019\001\034\001\255\255\
\022\001\023\001\034\001\255\255\255\255\041\001\042\001\255\255\
\044\001\045\001\042\001\255\255\044\001\045\001\016\001\017\001\
\018\001\019\001\255\255\255\255\022\001\023\001"

let yynames_const = "\
  LBRACK\000\
  RBRACK\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  SEMI\000\
  ASSIGN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  SERIAL\000\
  PARALLEL\000\
  VIB\000\
  TREM\000\
  BEND\000\
  ARROW\000\
  EQ\000\
  NEQ\000\
  INCR\000\
  DECR\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  LOOP\000\
  RETURN\000\
  INT\000\
  FUN\000\
  VOL\000\
  DUR\000\
  PITCH\000\
  INSTR\000\
  NOTE\000\
  REST\000\
  CHORD\000\
  TRACK\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                 ( [], [] )
# 403 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 45 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 411 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 46 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 419 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 51 "parser.mly"
    ( { fname = _1;
	formals = _3;
	locals = List.rev _6;
	body = List.rev _7 } )
# 432 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                  ( [] )
# 438 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 60 "parser.mly"
                  ( List.rev _1 )
# 445 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                         ( [_1] )
# 452 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                         ( _3 :: _1 )
# 460 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 69 "parser.mly"
              ( _2 )
# 467 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 70 "parser.mly"
                 ( _2 )
# 474 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 71 "parser.mly"
                  ( _2 )
# 481 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 72 "parser.mly"
                  ( _2 )
# 488 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                     ( [] )
# 494 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 76 "parser.mly"
                     ( _2 :: _1 )
# 502 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 81 "parser.mly"
    ( NOTE_CR(_2, _4, _6, _8) )
# 512 "parser.ml"
               : 'note_cr))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
                  ( CHORD_CR ([]) )
# 518 "parser.ml"
               : 'chord_cr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'chord_list) in
    Obj.repr(
# 86 "parser.mly"
                               ( CHORD_CR ( List.rev _2 ) )
# 525 "parser.ml"
               : 'chord_cr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
       ( [_1] )
# 532 "parser.ml"
               : 'chord_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'chord_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
                             ( _3 :: _1 )
# 540 "parser.ml"
               : 'chord_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'note_attribute) in
    Obj.repr(
# 95 "parser.mly"
                          ( ACCESSOR(_1, _3) )
# 548 "parser.ml"
               : 'accessor))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
        (Pitch)
# 554 "parser.ml"
               : 'note_attribute))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
        (Vol)
# 560 "parser.ml"
               : 'note_attribute))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
        (Dur)
# 566 "parser.ml"
               : 'note_attribute))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
          (Instr)
# 572 "parser.ml"
               : 'note_attribute))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
              ( Expr(_1) )
# 579 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( Return(_2) )
# 586 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 110 "parser.mly"
                            ( Block(List.rev _2) )
# 593 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 111 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 601 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 112 "parser.mly"
                                            ( If(_3, _5, _7) )
# 610 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 114 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 620 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 115 "parser.mly"
                                  ( While(_3, _5) )
# 628 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                   ( [] )
# 634 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 120 "parser.mly"
                   ( _2 :: _1 )
# 642 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                  ( Noexpr )
# 648 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                  ( _1 )
# 655 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 129 "parser.mly"
                     ( Literal(_1) )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parser.mly"
                     ( Id(_1) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'chord_cr) in
    Obj.repr(
# 131 "parser.mly"
                     ( _1 )
# 676 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'note_cr) in
    Obj.repr(
# 132 "parser.mly"
                     ( _1 )
# 683 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'accessor) in
    Obj.repr(
# 133 "parser.mly"
                      (_1)
# 690 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 698 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 706 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 730 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 738 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 746 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 754 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 762 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 770 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                     ( Binop(_1, Incr,   _3) )
# 778 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                     ( Binop(_1, Decr,   _3) )
# 786 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                     ( Binop(_1, Arrow,   _3) )
# 794 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                     ( Assign(_1, _3))
# 802 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                     ( Assign(_1, _3))
# 810 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                     ( Binop(_1, Ser, _3) )
# 818 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "parser.mly"
                       ( Binop (_1, Par, _3) )
# 826 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 151 "parser.mly"
                     ( Modifier(_1, Vib) )
# 833 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
                     ( Modifier(_1, Trem) )
# 840 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
                     ( Modifier(_1, Bend) )
# 847 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 154 "parser.mly"
                                 ( Call(_1, _3) )
# 855 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 155 "parser.mly"
                       ( _2 )
# 862 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "parser.mly"
                  ( [] )
# 868 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 161 "parser.mly"
                  ( List.rev _1 )
# 875 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "parser.mly"
                            ( [_1] )
# 882 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                            ( _3 :: _1 )
# 890 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
