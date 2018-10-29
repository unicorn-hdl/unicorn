/* Ocamlyacc parser for UNI-corn*/

%{
open Ast 
%}
%token OPAREN CPAREN OCURL CCURL OSQUARED CSQUARED 
%token COMMA
%token SEMI UNICORN NEWLINE EOF 
%token OGENERIC CGENERIC 
%token ASSIGN REGASSIGN 
%token LINECOMMENT OBLOCK CBLOCK
%token PLUS MINUS
%token PLUSDOT TIMESDOT
%token FOR TO FROM
%token OUT INIT
%token AND OR NOT NAND NOR XOR XNOR
%token PRINT
%token ONE ZERO
%token <int> LITERAL
%token <bool list> BOOLLIT
%token <string> ID

%right ASSIGN REGASSIGN
%left OSQUARED CSQUARED
%left XNOR XOR
%left OR NOR PLUSDOT
%left AND NAND TIMESDOT
%left NOT
%left PLUS MINUS
%left OPAREN CPAREN

%start program
%type <Ast.program> program

%%

program:
 | modulezList UNICORN EOF { $1 }

structureList:
 | /* Nothing */ { [] }
 | structureList structure { $2::$1 }

structure:
 | line  {$1}
 | loop  {$1}

argList:
 | /*Nothing*/ { [] }
 | argList COMMA arg { $3::$1 }
 | arg { $1::[] }
 
arg:
 | ID{($1 , Literal(1))}
 | ID OSQUARED LITERAL CSQUARED { $1 , Literal($3) }
 | ID OGENERIC ID CGENERIC { $1 , IntId($3)}

line: 
 | assignment SEMI {}
 | call  SEMI {} 
 | print SEMI {} 
 | declare SEMI {}
 | OUT assignment SEMI{}

boolval:
 | ONE {true}
 | ZERO {false}

/*may need to add register constructor from ast*/
assignment:
 | ID REGASSIGN binExprz INIT boolval{ Assign(true, $1, Literal(0), Literal(1), $3, $5) }
 | ID ASSIGN binExprz { Assign (false, $1, Literal(0), IntId($1^"Max"), $3, false) }
 
 | ID OSQUARED intExprz CSQUARED ASSIGN binExprz { Assign(false, $1, $3, $3, $6, false) } 

 binExprz:
 | LITERAL { Buslit($1) } 
 | binExprz PLUSDOT binExprz { BoolBinop($1, Or, $3) }
 | binExprz TIMESDOT binExprz { BoolBinop($1, And, $3) }
 | binExprz XOR binExprz { BoolBinop($1, Xor, $3) }
 | binExprz XNOR binExprz { BoolBinop($1, Xnor, $3) }
 | binExprz NAND binExprz { BoolBinop($1, Nand, $3) }
 | binExprz AND binExprz { BoolBinop($1, And, $3) }
 | binExprz OR binExprz { BoolBinop($1, Or, $3) }
 | NOT binExprz { Unop($2) }
 | binExprz NOR binExprz { BoolBinop($1, Nor, $3) }
 | ID { boolId($1) } 
 intExprz:
 | LITERAL { Literal($1) }
 | intExprz PLUS intExprz { IntBinop($1, Add, $3) }
 | intExprz MINUS intExprz { IntBinop($1, Sub, $3) }
 | ID { IntId($1) }

call:
 | ID OPAREN argList CPAREN {}

print: 
 | PRINT ID {}

declare:
 | ID {}

modulez:
 | ID OPAREN argList CPAREN OCURL structureList CCURL {makeModule($3, $6)}

modulezList:
 | /*Nothing*/{}
 | modulez modulezList{}

varNum:
 | ID { StringLit($1) }
 | LITERAL { Literal($1) }
 | varNum PLUS varNum {$1 + $3}
 | varNum MINUS varNum {$1 - $3}

loop:
 | FOR OPAREN ID FROM varNum TO varNum CPAREN OCURL loopBody CCURL {}
 | FOR OPAREN ID TO varNum CPAREN OCURL loopBody CCURL {}

loopBody:
 | structure {}
