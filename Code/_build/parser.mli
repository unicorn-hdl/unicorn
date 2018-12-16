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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
