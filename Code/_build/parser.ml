type token =
  | OPAREN
  | CPAREN
  | OCURL
  | CCURL
  | OSQUARED
  | CSQUARED
  | COMMA
  | COLON
  | SEMI
  | UNICORN
  | UNICORN2
  | EOF
  | OGENERIC
  | CGENERIC
  | ASSIGN
  | REGASSIGN
  | STAR
  | PLUS
  | MINUS
  | PLUSDOT
  | TIMESDOT
  | FOR
  | TO
  | FROM
  | OUT
  | INIT
  | AND
  | OR
  | NOT
  | NAND
  | NOR
  | XOR
  | XNOR
  | PRINT
  | MAKE
  | LITERAL of (int)
  | BOOLLIT of (string)
  | ID of (string)

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast 
# 46 "parser.ml"
let yytransl_const = [|
  257 (* OPAREN *);
  258 (* CPAREN *);
  259 (* OCURL *);
  260 (* CCURL *);
  261 (* OSQUARED *);
  262 (* CSQUARED *);
  263 (* COMMA *);
  264 (* COLON *);
  265 (* SEMI *);
  266 (* UNICORN *);
  267 (* UNICORN2 *);
    0 (* EOF *);
  268 (* OGENERIC *);
  269 (* CGENERIC *);
  270 (* ASSIGN *);
  271 (* REGASSIGN *);
  272 (* STAR *);
  273 (* PLUS *);
  274 (* MINUS *);
  275 (* PLUSDOT *);
  276 (* TIMESDOT *);
  277 (* FOR *);
  278 (* TO *);
  279 (* FROM *);
  280 (* OUT *);
  281 (* INIT *);
  282 (* AND *);
  283 (* OR *);
  284 (* NOT *);
  285 (* NAND *);
  286 (* NOR *);
  287 (* XOR *);
  288 (* XNOR *);
  289 (* PRINT *);
  290 (* MAKE *);
    0|]

let yytransl_block = [|
  291 (* LITERAL *);
  292 (* BOOLLIT *);
  293 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\004\000\004\000\004\000\
\007\000\008\000\008\000\010\000\010\000\009\000\009\000\009\000\
\009\000\009\000\005\000\005\000\011\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\013\000\
\013\000\018\000\018\000\014\000\019\000\019\000\019\000\015\000\
\016\000\017\000\017\000\006\000\000\000"

let yylen = "\002\000\
\003\000\003\000\000\000\002\000\008\000\000\000\003\000\001\000\
\002\000\000\000\003\000\003\000\005\000\003\000\003\000\003\000\
\001\000\001\000\000\000\002\000\002\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\001\000\002\000\001\000\001\000\001\000\001\000\001\000\004\000\
\003\000\003\000\003\000\004\000\000\000\003\000\001\000\004\000\
\003\000\011\000\009\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\053\000\000\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\008\000\002\000\001\000\000\000\
\009\000\000\000\000\000\000\000\017\000\018\000\000\000\019\000\
\007\000\000\000\011\000\000\000\000\000\000\000\016\000\014\000\
\015\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\
\000\000\000\000\020\000\000\000\035\000\036\000\037\000\038\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\031\000\000\000\
\000\000\000\000\049\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\052\000\000\000\044\000\000\000\012\000\000\000\
\000\000\040\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\042\000\019\000\000\000\000\000\000\000\051\000\
\019\000\000\000\050\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\012\000\030\000\042\000\013\000\017\000\
\023\000\070\000\043\000\044\000\045\000\046\000\047\000\048\000\
\049\000\098\000\077\000"

let yysindex = "\005\000\
\009\255\000\000\061\255\000\000\044\255\009\255\042\255\091\000\
\095\000\000\000\112\255\005\255\000\000\000\000\000\000\010\255\
\000\000\139\255\042\255\010\255\000\000\000\000\065\255\000\000\
\000\000\001\255\000\000\010\255\010\255\016\255\000\000\000\000\
\000\000\066\255\133\255\143\255\066\255\117\255\118\255\000\000\
\165\255\163\255\000\000\094\000\000\000\000\000\000\000\000\000\
\000\000\253\255\136\255\042\255\169\255\160\255\112\255\066\255\
\000\000\010\255\000\000\066\255\066\255\066\255\066\255\066\255\
\066\255\066\255\066\255\066\255\066\255\000\000\000\000\249\254\
\015\255\066\255\000\000\132\000\041\255\080\255\132\000\113\000\
\091\255\169\255\169\255\091\255\169\255\091\255\148\000\148\000\
\010\255\010\255\000\000\132\000\000\000\066\255\000\000\010\255\
\144\255\000\000\003\255\105\255\132\000\252\254\161\255\175\255\
\010\255\000\000\000\000\000\000\008\255\038\255\176\255\000\000\
\000\000\056\255\000\000"

let yyrindex = "\000\000\
\142\255\000\000\000\000\000\000\000\000\142\255\114\255\000\000\
\000\000\000\000\103\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\099\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\100\255\156\255\000\000\130\255\134\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\141\255\000\000\000\000\131\255\000\000\
\023\000\182\255\208\255\042\000\234\255\061\000\049\255\080\000\
\000\000\000\000\000\000\163\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\162\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\175\000\000\000\138\000\216\255\000\000\173\000\139\000\
\236\255\000\000\000\000\223\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yytablesize = 435
let yytable = "\026\000\
\050\000\106\000\031\000\053\000\104\000\001\000\018\000\032\000\
\033\000\111\000\020\000\019\000\028\000\029\000\089\000\090\000\
\034\000\028\000\029\000\028\000\029\000\019\000\076\000\091\000\
\028\000\029\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\035\000\078\000\034\000\036\000\
\092\000\112\000\093\000\037\000\021\000\003\000\022\000\094\000\
\038\000\039\000\025\000\040\000\041\000\008\000\009\000\025\000\
\034\000\025\000\035\000\115\000\101\000\007\000\025\000\025\000\
\025\000\037\000\034\000\110\000\099\000\100\000\038\000\039\000\
\114\000\040\000\041\000\102\000\035\000\027\000\011\000\025\000\
\025\000\028\000\029\000\037\000\109\000\095\000\035\000\096\000\
\038\000\039\000\014\000\040\000\041\000\037\000\015\000\058\000\
\028\000\029\000\038\000\039\000\033\000\040\000\041\000\033\000\
\010\000\033\000\006\000\033\000\006\000\010\000\063\000\010\000\
\033\000\033\000\033\000\006\000\064\000\033\000\033\000\066\000\
\006\000\028\000\029\000\016\000\033\000\033\000\105\000\033\000\
\033\000\033\000\033\000\010\000\041\000\051\000\010\000\045\000\
\010\000\041\000\010\000\041\000\045\000\024\000\047\000\010\000\
\010\000\010\000\041\000\047\000\010\000\010\000\052\000\003\000\
\003\000\054\000\055\000\010\000\010\000\032\000\010\000\010\000\
\010\000\010\000\032\000\046\000\032\000\056\000\057\000\074\000\
\046\000\032\000\032\000\032\000\072\000\058\000\032\000\032\000\
\107\000\108\000\113\000\103\000\010\000\032\000\032\000\024\000\
\032\000\032\000\032\000\032\000\024\000\073\000\024\000\025\000\
\000\000\075\000\000\000\024\000\024\000\024\000\000\000\000\000\
\024\000\024\000\000\000\000\000\000\000\000\000\000\000\024\000\
\024\000\028\000\024\000\024\000\024\000\024\000\028\000\000\000\
\028\000\000\000\000\000\000\000\000\000\028\000\028\000\028\000\
\000\000\000\000\028\000\028\000\000\000\000\000\000\000\000\000\
\000\000\028\000\028\000\027\000\028\000\028\000\028\000\028\000\
\027\000\000\000\027\000\000\000\000\000\000\000\000\000\027\000\
\027\000\027\000\000\000\000\000\027\000\027\000\071\000\000\000\
\000\000\058\000\000\000\027\000\027\000\000\000\027\000\027\000\
\027\000\027\000\060\000\061\000\000\000\000\000\000\000\062\000\
\063\000\000\000\000\000\000\000\000\000\000\000\064\000\065\000\
\023\000\066\000\067\000\068\000\069\000\023\000\000\000\023\000\
\000\000\000\000\000\000\000\000\023\000\023\000\023\000\000\000\
\000\000\023\000\000\000\029\000\000\000\000\000\000\000\000\000\
\029\000\023\000\029\000\000\000\023\000\023\000\023\000\029\000\
\029\000\029\000\000\000\000\000\029\000\000\000\030\000\000\000\
\000\000\000\000\000\000\030\000\029\000\030\000\000\000\029\000\
\029\000\029\000\030\000\030\000\030\000\000\000\000\000\030\000\
\000\000\026\000\000\000\000\000\000\000\000\000\026\000\030\000\
\026\000\000\000\030\000\030\000\030\000\026\000\026\000\026\000\
\000\000\000\000\058\000\000\000\000\000\000\000\059\000\000\000\
\000\000\000\000\000\000\060\000\061\000\000\000\026\000\026\000\
\062\000\063\000\000\000\000\000\000\000\058\000\000\000\064\000\
\065\000\000\000\066\000\067\000\068\000\069\000\060\000\061\000\
\097\000\000\000\000\000\062\000\063\000\000\000\000\000\000\000\
\058\000\000\000\064\000\065\000\000\000\066\000\067\000\068\000\
\069\000\060\000\061\000\000\000\000\000\000\000\062\000\063\000\
\058\000\000\000\000\000\000\000\000\000\064\000\065\000\000\000\
\066\000\067\000\068\000\069\000\048\000\000\000\062\000\063\000\
\000\000\048\000\000\000\048\000\000\000\064\000\065\000\000\000\
\066\000\067\000\048\000"

let yycheck = "\020\000\
\034\000\006\001\002\001\037\000\002\001\001\000\002\001\028\000\
\029\000\002\001\001\001\007\001\017\001\018\001\022\001\023\001\
\001\001\017\001\018\001\017\001\018\001\007\001\056\000\009\001\
\017\001\018\001\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\021\001\058\000\001\001\024\001\
\074\000\004\001\002\001\028\001\035\001\037\001\037\001\007\001\
\033\001\034\001\002\001\036\001\037\001\010\001\011\001\007\001\
\001\001\009\001\021\001\004\001\094\000\001\001\014\001\015\001\
\016\001\028\001\001\001\108\000\089\000\090\000\033\001\034\001\
\113\000\036\001\037\001\096\000\021\001\013\001\037\001\031\001\
\032\001\017\001\018\001\028\001\105\000\006\001\021\001\008\001\
\033\001\034\001\000\000\036\001\037\001\028\001\000\000\005\001\
\017\001\018\001\033\001\034\001\002\001\036\001\037\001\005\001\
\002\001\007\001\007\001\009\001\009\001\007\001\020\001\009\001\
\014\001\015\001\016\001\002\001\026\001\019\001\020\001\029\001\
\007\001\017\001\018\001\012\001\026\001\027\001\022\001\029\001\
\030\001\031\001\032\001\002\001\002\001\001\001\005\001\002\001\
\007\001\007\001\009\001\009\001\007\001\003\001\002\001\014\001\
\015\001\016\001\016\001\007\001\019\001\020\001\008\001\010\001\
\011\001\037\001\037\001\026\001\027\001\002\001\029\001\030\001\
\031\001\032\001\007\001\002\001\009\001\001\001\004\001\008\001\
\007\001\014\001\015\001\016\001\037\001\005\001\019\001\020\001\
\016\001\003\001\003\001\036\001\006\000\026\001\027\001\002\001\
\029\001\030\001\031\001\032\001\007\001\052\000\009\001\019\000\
\255\255\055\000\255\255\014\001\015\001\016\001\255\255\255\255\
\019\001\020\001\255\255\255\255\255\255\255\255\255\255\026\001\
\027\001\002\001\029\001\030\001\031\001\032\001\007\001\255\255\
\009\001\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\255\255\255\255\019\001\020\001\255\255\255\255\255\255\255\255\
\255\255\026\001\027\001\002\001\029\001\030\001\031\001\032\001\
\007\001\255\255\009\001\255\255\255\255\255\255\255\255\014\001\
\015\001\016\001\255\255\255\255\019\001\020\001\002\001\255\255\
\255\255\005\001\255\255\026\001\027\001\255\255\029\001\030\001\
\031\001\032\001\014\001\015\001\255\255\255\255\255\255\019\001\
\020\001\255\255\255\255\255\255\255\255\255\255\026\001\027\001\
\002\001\029\001\030\001\031\001\032\001\007\001\255\255\009\001\
\255\255\255\255\255\255\255\255\014\001\015\001\016\001\255\255\
\255\255\019\001\255\255\002\001\255\255\255\255\255\255\255\255\
\007\001\027\001\009\001\255\255\030\001\031\001\032\001\014\001\
\015\001\016\001\255\255\255\255\019\001\255\255\002\001\255\255\
\255\255\255\255\255\255\007\001\027\001\009\001\255\255\030\001\
\031\001\032\001\014\001\015\001\016\001\255\255\255\255\019\001\
\255\255\002\001\255\255\255\255\255\255\255\255\007\001\027\001\
\009\001\255\255\030\001\031\001\032\001\014\001\015\001\016\001\
\255\255\255\255\005\001\255\255\255\255\255\255\009\001\255\255\
\255\255\255\255\255\255\014\001\015\001\255\255\031\001\032\001\
\019\001\020\001\255\255\255\255\255\255\005\001\255\255\026\001\
\027\001\255\255\029\001\030\001\031\001\032\001\014\001\015\001\
\016\001\255\255\255\255\019\001\020\001\255\255\255\255\255\255\
\005\001\255\255\026\001\027\001\255\255\029\001\030\001\031\001\
\032\001\014\001\015\001\255\255\255\255\255\255\019\001\020\001\
\005\001\255\255\255\255\255\255\255\255\026\001\027\001\255\255\
\029\001\030\001\031\001\032\001\002\001\255\255\019\001\020\001\
\255\255\007\001\255\255\009\001\255\255\026\001\027\001\255\255\
\029\001\030\001\016\001"

let yynames_const = "\
  OPAREN\000\
  CPAREN\000\
  OCURL\000\
  CCURL\000\
  OSQUARED\000\
  CSQUARED\000\
  COMMA\000\
  COLON\000\
  SEMI\000\
  UNICORN\000\
  UNICORN2\000\
  EOF\000\
  OGENERIC\000\
  CGENERIC\000\
  ASSIGN\000\
  REGASSIGN\000\
  STAR\000\
  PLUS\000\
  MINUS\000\
  PLUSDOT\000\
  TIMESDOT\000\
  FOR\000\
  TO\000\
  FROM\000\
  OUT\000\
  INIT\000\
  AND\000\
  OR\000\
  NOT\000\
  NAND\000\
  NOR\000\
  XOR\000\
  XNOR\000\
  PRINT\000\
  MAKE\000\
  "

let yynames_block = "\
  LITERAL\000\
  BOOLLIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'modulezList) in
    Obj.repr(
# 37 "parser.mly"
                            ( _1 )
# 336 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'modulezList) in
    Obj.repr(
# 38 "parser.mly"
                            ( _1 )
# 343 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
              ( [] )
# 349 "parser.ml"
               : 'modulezList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'modulez) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'modulezList) in
    Obj.repr(
# 42 "parser.mly"
                      ( _1::_2 )
# 357 "parser.ml"
               : 'modulezList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formalsList) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'lineList) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'outlist) in
    Obj.repr(
# 46 "parser.mly"
            ( MD(_7, _1, _3, _6) )
# 367 "parser.ml"
               : 'modulez))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                 ( [] )
# 373 "parser.ml"
               : 'formalsList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formalsList) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 50 "parser.mly"
                           ( _3::_1 )
# 381 "parser.ml"
               : 'formalsList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 51 "parser.mly"
          ( [_1] )
# 388 "parser.ml"
               : 'formalsList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typdecl) in
    Obj.repr(
# 54 "parser.mly"
             ( (_2, _1) )
# 396 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                                   (Lit(1))
# 402 "parser.ml"
               : 'typdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intExprz) in
    Obj.repr(
# 58 "parser.mly"
                              (_2)
# 409 "parser.ml"
               : 'typdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intExprz) in
    Obj.repr(
# 67 "parser.mly"
                              (Range(_2, _2))
# 416 "parser.ml"
               : 'index))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'intExprz) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'intExprz) in
    Obj.repr(
# 68 "parser.mly"
                                             (Range(_2, _4))
# 424 "parser.ml"
               : 'index))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'intExprz) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'intExprz) in
    Obj.repr(
# 71 "parser.mly"
                          ( IntBinop(_1, Add, _3) )
# 432 "parser.ml"
               : 'intExprz))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'intExprz) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'intExprz) in
    Obj.repr(
# 72 "parser.mly"
                           ( IntBinop(_1, Sub, _3) )
# 440 "parser.ml"
               : 'intExprz))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intExprz) in
    Obj.repr(
# 73 "parser.mly"
                          ( _2 )
# 447 "parser.ml"
               : 'intExprz))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
           ( Lit(_1) )
# 454 "parser.ml"
               : 'intExprz))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
      ( IntId(_1) )
# 461 "parser.ml"
               : 'intExprz))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                 ( [] )
# 467 "parser.ml"
               : 'lineList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lineList) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 79 "parser.mly"
                 ( _2::_1 )
# 475 "parser.ml"
               : 'lineList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'binExpr) in
    Obj.repr(
# 82 "parser.mly"
               (_1)
# 482 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
           ( Buslit(_1) )
# 489 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 86 "parser.mly"
                           ( BoolBinop(_1, Or, _3) )
# 497 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 87 "parser.mly"
                            ( BoolBinop(_1, And, _3) )
# 505 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 88 "parser.mly"
                       ( BoolBinop(_1, Xor, _3) )
# 513 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 89 "parser.mly"
                        ( BoolBinop(_1, Xnor, _3) )
# 521 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 90 "parser.mly"
                        ( BoolBinop(_1, Nand, _3) )
# 529 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 91 "parser.mly"
                       ( BoolBinop(_1, And, _3) )
# 537 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 92 "parser.mly"
                      ( BoolBinop(_1, Or, _3) )
# 545 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 93 "parser.mly"
                       ( BoolBinop(_1, Nor, _3) )
# 553 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'binExpr) in
    Obj.repr(
# 94 "parser.mly"
                         ( _2 )
# 560 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 95 "parser.mly"
               ( Unop(Not, _2) )
# 567 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
      ( BoolId(_1) )
# 574 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'binExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'index) in
    Obj.repr(
# 97 "parser.mly"
                 ( Index(_1, _2) )
# 582 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment) in
    Obj.repr(
# 99 "parser.mly"
              (_1)
# 589 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'call) in
    Obj.repr(
# 100 "parser.mly"
        (_1)
# 596 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'print) in
    Obj.repr(
# 101 "parser.mly"
         (_1)
# 603 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declare) in
    Obj.repr(
# 102 "parser.mly"
           (_1)
# 610 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'loop) in
    Obj.repr(
# 103 "parser.mly"
        (_1)
# 617 "parser.ml"
               : 'binExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'binExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'boolval) in
    Obj.repr(
# 106 "parser.mly"
                                    ( Assign(true, _1, _3, _4) )
# 626 "parser.ml"
               : 'assignment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 107 "parser.mly"
                             ( Assign(false, _1, _3, "0") )
# 634 "parser.ml"
               : 'assignment))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 110 "parser.mly"
                     (_2)
# 641 "parser.ml"
               : 'boolval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 111 "parser.mly"
                     (_2)
# 648 "parser.ml"
               : 'boolval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'argList) in
    Obj.repr(
# 114 "parser.mly"
                            (Call(_1, _3))
# 656 "parser.ml"
               : 'call))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
               ( [] )
# 662 "parser.ml"
               : 'argList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argList) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 118 "parser.mly"
                         ( _3::_1 )
# 670 "parser.ml"
               : 'argList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 119 "parser.mly"
           ( _1::[] )
# 677 "parser.ml"
               : 'argList))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'binExpr) in
    Obj.repr(
# 122 "parser.mly"
                          ( Print(_2, _4) )
# 685 "parser.ml"
               : 'print))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typdecl) in
    Obj.repr(
# 125 "parser.mly"
                   (BoolId(_2))
# 693 "parser.ml"
               : 'declare))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'intExprz) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'intExprz) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'lineList) in
    Obj.repr(
# 128 "parser.mly"
                                                                       (
            For(_3, Range(_5, _7), _10))
# 704 "parser.ml"
               : 'loop))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'intExprz) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'lineList) in
    Obj.repr(
# 130 "parser.mly"
                                                         (
            For(_3, Range(Lit(0), _5), _8))
# 714 "parser.ml"
               : 'loop))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formalsList) in
    Obj.repr(
# 134 "parser.mly"
                              (_3)
# 721 "parser.ml"
               : 'outlist))
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
