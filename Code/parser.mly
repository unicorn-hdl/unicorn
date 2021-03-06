/* Ocamlyacc parser for UNI-corn*/

%{
open Ast 
%}

%token OPAREN CPAREN OCURL CCURL OSQUARED CSQUARED 
%token COMMA COLON
%token SEMI UNICORN UNICORN2 EOF 
%token OGENERIC CGENERIC 
%token ASSIGN REGASSIGN STAR
%token PLUS MINUS POUND
%token EXCITEDSTAR EXCITEDPLUS EXCITEDPOUND WOW
%token FOR TO FROM
%token OUT INIT
%token AND OR NOT NAND NOR XOR XNOR
%token PRINT MAKE 
%token <int> LITERAL
%token <string> BOOLLIT
%token <string> ID

%left COLON
%right ASSIGN REGASSIGN
%left XNOR XOR POUND EXCITEDPOUND
%left OR NOR PLUS EXCITEDPLUS
%left AND NAND STAR EXCITEDSTAR
%left NOT WOW
%left MINUS
%left OSQUARED

%start program
%type <Ast.program> program

%%

program:
 | modulezList UNICORN2 EOF { $1 }
 | modulezList UNICORN  EOF { $1 }

modulezList:
 | /*Nothing*/{ [] }
 | modulez modulezList{ $1::$2 }
 
modulez:
 | ID OPAREN formalsList CPAREN OCURL lineList outlist CCURL 
            { MD($7, $1, $3, $6) }

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
 | binExpr AND binExpr { BoolBinop($1, And, $3) }
 | binExpr OR binExpr { BoolBinop($1, Or, $3) }
 | binExpr XOR binExpr { BoolBinop($1, Xor, $3) }
 | binExpr STAR binExpr { BoolBinop($1, And, $3) }
 | binExpr PLUS binExpr { BoolBinop($1, Or, $3) }
 | binExpr POUND binExpr { BoolBinop($1, Xor, $3) }
 | binExpr NAND binExpr { Unop(Not, BoolBinop($1, And, $3)) }
 | binExpr NOR binExpr { Unop(Not, BoolBinop($1, Or, $3)) }
 | binExpr XNOR binExpr { Unop(Not, BoolBinop($1, Xor, $3)) }
 | binExpr EXCITEDPOUND binExpr { Unop(Not, BoolBinop($1, Xor, $3)) }
 | binExpr EXCITEDSTAR binExpr { Unop(Not, BoolBinop($1, And, $3)) }
 | binExpr EXCITEDPLUS binExpr { Unop(Not, BoolBinop($1, Or, $3)) }
 | OPAREN binExpr CPAREN { $2 } 
 | NOT binExpr { Unop(Not, $2) }
 | WOW binExpr { Unop(Not, $2)}
 | ID { BoolId($1) } 
 | binExpr index { Index($1, $2) }
 /*note these other important things are exprs too: */
 | assignment {$1}
 | call {$1}
 | print {$1}
 | loop {$1}

assignment:
 | binExpr REGASSIGN binExpr boolval{ Assign(true, $1, $3, $4) }
 | binExpr  ASSIGN   binExpr { Assign(false, $1, $3, "0") } 

boolval:
 | STAR BOOLLIT STAR {$2}
 | STAR BOOLLIT STAR {$2}

call:
 | ID OPAREN argList CPAREN {Call($1, $3)}

argList:
 | /*Nothing*/ { [] }
 | argList COMMA binExpr { $3::$1 }
 | binExpr { $1::[] }

print: 
 | PRINT ID COLON binExpr { Print($2, $4) }

loop:
 | FOR OPAREN ID FROM intExprz TO intExprz CPAREN OCURL lineList CCURL {
            For($3, Range($5, $7), $10)}
 | FOR OPAREN ID TO intExprz CPAREN OCURL lineList CCURL {
            For($3, Range(Lit(0), $5), $8)}

outlist:
 | OUT COLON formalsList SEMI {$3}

