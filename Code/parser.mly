/* Ocamlyacc parser for UNI-corn*/

%{
open Ast 
%}
%token OPAREN CPAREN OCURL CCURL OSQUARED CSQUARED 
<<<<<<< HEAD
%token COMMA COLON
%token SEMI UNICORN UNICORN2 EOF 
%token OGENERIC CGENERIC 
=======
%token COMMA
<<<<<<< HEAD
%token SEMI UNICORN NEWLINE EOF %token OGENERIC CGENERIC 
>>>>>>> b3fcd7c... Put made support for comments. Put in some { } things.
=======
%token SEMI UNICORN NEWLINE EOF 
%token OGENERIC CGENERIC 
>>>>>>> b1378ab... fixed token newline
%token ASSIGN REGASSIGN 
%token PLUS MINUS
%token PLUSDOT TIMESDOT
%token FOR TO FROM
%token OUT INIT
%token AND OR NOT NAND NOR XOR XNOR
<<<<<<< HEAD
%token PRINT MAKE 
=======
%token MAIN
%token PRINT
>>>>>>> 60a67d8... Got rid of module keyword
%token ONE ZERO
%token <int> LITERAL
%token <string> BOOLLIT
%token <string> ID

%left COLON
%right ASSIGN REGASSIGN
%left XNOR XOR
%left OR NOR PLUSDOT
%left AND NAND TIMESDOT
%left NOT
%left PLUS MINUS
%left OSQUARED

%start program
%type <Ast.program> program

%%

program:
<<<<<<< HEAD
<<<<<<< HEAD
 | modulezList UNICORN  EOF { $1 }
 | modulezList UNICORN2 EOF { $1 }

modulezList:
 | /*Nothing*/{ [] }
 | modulez modulezList{ $1::$2 }
 
modulez:
 | ID OPAREN formalsList CPAREN OCURL lineList outlist CCURL 
            { Module_decl($7, $1, $3, $6) }

formalsList:
 | /* Nothing */ { [] }
 | formalsList COMMA formal{ $3::$1 }
 | formal { [$1] }

formal:
 | ID typdecl{ ($2, $1) }
 
typdecl:
 | /*Nothing (reduces to size 1)*/ {Lit(1)}
 | OGENERIC intExprz CGENERIC {$2}

/*
opt_index:
 |*/ /*Nothing (whole bus)*//* {Range(Lit(0),Lit(-1))}
 | index {$1}
*/

index:
 | OSQUARED intExprz CSQUARED {Range($2, $2)}
 | OSQUARED intExprz COLON intExprz CSQUARED {Range($2, $4)}

intExprz:
 | intExprz PLUS intExprz { IntBinop($1, Add, $3) }
 | intExprz MINUS intExprz { IntBinop($1, Sub, $3) }
 | OPAREN intExprz CPAREN { $2 }
 | LITERAL { Lit($1) }
 | ID { IntId($1) }

lineList:
 | /* Nothing */ { [] }
 | lineList line { $2::$1 }

line:
 | binExpr SEMI{$1}

binExpr:
 | BOOLLIT { Buslit($1) } 
 | binExpr PLUSDOT binExpr { BoolBinop($1, Or, $3) }
 | binExpr TIMESDOT binExpr { BoolBinop($1, And, $3) }
 | binExpr XOR binExpr { BoolBinop($1, Xor, $3) }
 | binExpr XNOR binExpr { BoolBinop($1, Xnor, $3) }
 | binExpr NAND binExpr { BoolBinop($1, Nand, $3) }
 | binExpr AND binExpr { BoolBinop($1, And, $3) }
 | binExpr OR binExpr { BoolBinop($1, Or, $3) }
 | binExpr NOR binExpr { BoolBinop($1, Nor, $3) }
 | OPAREN binExpr CPAREN { $2 } 
 | NOT binExpr { Unop(Not, $2) }
 | ID { BoolId($1) } 
 | binExpr index { Index($1, $2) }
 /*note these other important things are exprs too: */
 | assignment {$1}
 | call {$1}
 | print {Buslit("0b")}
 | declare {$1}
 | loop {$1}

assignment:
<<<<<<< HEAD
 | binExpr REGASSIGN binExpr boolval{ Assign(true, $1, $3, $4) }
 | binExpr ASSIGN binExpr { Assign(false, $1, $3, false) } 
=======
 | comment mainz modulezList UNICORN EOF {bigTree($1, $2)}
=======
 | mainz modulezList UNICORN EOF {bigTree($1, $2)}
>>>>>>> 46fcfed... Fixed the dumb comment thing I'd done

mainz:
 | MAIN OPAREN argList CPAREN OCURL structureList CCURL {makeMain($3, $6)}
 
structureList:
 | /* Nothing */ {}
 | structure structureList {listIs($1::$2)}

structure:
 | line  {$1}
 | loop  {$1}
=======
 | binExpr REGASSIGN binExpr INIT boolval{ Assign(true, $1, $3, $5) }
 | binExpr ASSIGN binExpr { Assign(false, $1, $3, false) } 
>>>>>>> 1f1c4a5... SAME THING PUT I Pulled

argList:
 | /*Nothing*/ {}
 | argList COMMA arg {argListIs($1,$3)}
 
arg:
 | ID{literalSize($1, 1)}
 | ID OSQUARED LITERAL CSQUARED {literalSize($1, $3)}
 | ID OGENERIC ID CGENERIC {genericSize($1, $3)}

line: 
 | assignment SEMI {}
 | call  SEMI {} 
 | print SEMI {} 
 | declare SEMI {}
 | OUT assignment SEMI{}

<<<<<<< HEAD
comment:
 | OBLOCK anythingList  CBLOCK{}
>>>>>>> b3fcd7c... Put made support for comments. Put in some { } things.

=======
>>>>>>> 46fcfed... Fixed the dumb comment thing I'd done
boolval:
 | ONE {true}
 | ZERO {false}

<<<<<<< HEAD
call:
 | ID OPAREN argList CPAREN {Call($1, $3)}

argList:
 | /*Nothing*/ { [] }
 | argList COMMA binExpr { $3::$1 }
 | binExpr { $1::[] }

print: 
 | PRINT ID COLON binExpr { Print($2, $4) }

declare:
 | MAKE ID typdecl {BoolId($2)}

loop:
 | FOR OPAREN ID FROM intExprz TO intExprz CPAREN OCURL lineList CCURL {
            For($3, Range($5, $7), $10)}
 | FOR OPAREN ID TO intExprz CPAREN OCURL lineList CCURL {
            For($3, Range(Lit(0), $5), $8)}

outlist:
 | OUT COLON formalsList SEMI {$3}

=======
assignment:
 | ID REGASSIGN ID INIT boolval{ RegisterAssign($1, $5, $3)} 
 | ID ASSIGN ID OPAREN argList CPAREN { WireAssign($1, valueOf($3, $5))}
 | ID ASSIGN ID OPAREN argList CPAREN OSQUARED ID CSQUARED {WireAssign($1, ValueOf($3, $5, $8)) }
 | ID ASSIGN ID OPAREN argList CPAREN OSQUARED ID CSQUARED OSQUARED CSQUARED {WireAssign($1, ValueOf($3, $5, $8))}

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
<<<<<<< HEAD

anything:
 | OPAREN{} | CPAREN{} | OCURL{} | CCURL {}| OSQUARED {}| CSQUARED {}
 | COMMA{} | SEMI{} | UNICORN{} | OGENERIC {}| CGENERIC{} | ASSIGN{} | REGASSIGN {}
 | LINECOMMENT{} | OBLOCK{}
 | PLUS {}| MINUS{}
 | FOR {}| TO {}| FROM{}
 | OUT {}| INIT{}
 | AND {}| OR {}| NOT {}| NAND {}| NOR {}| XOR {}| XNOR{}
 | MAIN{}
 | PRINT{}
 | ONE {}| ZERO{}
 | LITERAL{}
 | ID{}
 | NEWLINE {}

anythingList:
 | /*Nothing*/ {} 
 | anything anythingList {}
>>>>>>> b3fcd7c... Put made support for comments. Put in some { } things.
=======
>>>>>>> 46fcfed... Fixed the dumb comment thing I'd done
