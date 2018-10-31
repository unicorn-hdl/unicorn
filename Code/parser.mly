/* Ocamlyacc parser for UNI-corn*/

%{
open Ast 
%}
%token OPAREN CPAREN OCURL CCURL OSQUARED CSQUARED 
%token COMMA COLON
%token SEMI UNICORN UNICORN2 NEWLINE EOF 
%token OGENERIC CGENERIC 
%token ASSIGN REGASSIGN 
%token LINECOMMENT OBLOCK CBLOCK
%token PLUS MINUS
%token PLUSDOT TIMESDOT
%token FOR TO FROM
%token OUT INIT
%token AND OR NOT NAND NOR XOR XNOR
%token PRINT MAKE 
%token ONE ZERO
%token <int> LITERAL
%token <string> BOOLLIT
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
 | modulezList UNICORN  EOF { ([], $1)}
 | modulezList UNICORN2 EOF { ([], $1) }

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
 | ID typdecl{ (Bus, $2, $1) }
 
typdecl:
 | /*Nothing (reduces to size 1)*/ {Lit(1)}
 | OGENERIC intExpr CGENERIC {$2}

index:
 | /*Nothing (whole bus)*/ {Range(Lit(0),Lit(-1))}
 | OSQUARED intExpr CSQUARED {Range($2, $2)}
 | OSQUARED intExpr COLON intExpr CSQUARED {Range($2, $4)}

intExpr:
 | intExpr PLUS intExpr { IntBinop($1, Add, $3) }
 | intExpr MINUS intExpr { IntBinop($1, Sub, $3) }
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
 | NOT binExpr { Unop(Not, $2) }
 | binExpr NOR binExpr { BoolBinop($1, Nor, $3) }
 | ID { BoolId($1) } 
 /*note these other important things are exprs too: */
 | assignment {$1}
 | call {$1}
 | print {$1}
 | declare {$1}
 | loop {$1}

/*may need to add register constructor from ast*/ assignment:
 | ID index REGASSIGN binExpr INIT boolval{ 
            Assign(true, $1, $2, $4, $6) }
 | ID index ASSIGN binExpr {
            Assign(false, $1, $2, $4, false) } 

boolval:
 | ONE {true}
 | ZERO {false}

call:
 | ID OPAREN argList CPAREN index {Call($1, $3, $5)}

argList:
 | /*Nothing*/ { [] }
 | argList COMMA binExpr { $3::$1 }
 | binExpr { $1::[] }

print: 
 | PRINT ID {BoolId($2)}

declare:
 | MAKE ID typdecl {BoolId($2)}

loop:
 | FOR OPAREN ID FROM intExpr TO intExpr CPAREN OCURL lineList CCURL {
            For($3, Range($5, $7), $10)}
 | FOR OPAREN ID TO intExpr CPAREN OCURL lineList CCURL {
            For($3, Range(Lit(0), $5), $8)}

outlist:
 | OUT COLON formalsList SEMI {$3}

