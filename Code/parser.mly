/* Ocamlyacc parser for UNI-corn*/

%{
open Ast 
%}
%token OPAREN CPAREN OCURL CCURL OSQUARED CSQUARED 
%token COMMA
%token SEMI UNICORN NEWLINE EOF %token OGENERIC CGENERIC 
%token ASSIGN REGASSIGN 
%token LINECOMMENT OBLOCK CBLOCK
%token PLUS MINUS
%token FOR TO FROM
%token OUT INIT
%token AND OR NOT NAND NOR XOR XNOR
%token MAIN
%token PRINT
%token ONE ZERO
%token <int> LITERAL
%token <string> ID


%right ASSIGN REGASSIGN
%left OSQUARED CSQUARED
%left PLUS MINUS
%left OPAREN CPAREN

%start program
%type <Ast.program> program

%%

program:
 | comment mainz modulezList UNICORN EOF {bigTree($1, $2)}

mainz:
 | MAIN OPAREN argList CPAREN OCURL structureList CCURL {makeMain($3, $6)}
 
structureList:
 | /* Nothing */ {}
 | structure structureList {listIs($1::$2)}

structure:
 | line  {$1}
 | comment {}
 | loop  {$1}

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

comment:
 | OBLOCK anythingList  CBLOCK{}

boolval:
 | ONE {true}
 | ZERO {false}

assignment:
 | ID REGASSIGN ID INIT boolval{ RegisterAssign($1, $5, $3)} 
 | ID ASSIGN ID OPAREN argList CPAREN { WireAssign($1, valueOf($3, $5))}
 | ID ASSIGN ID OPAREN argList CPAREN OSQUARED ID CSQUARED {WireAssign($1, ValueOf($3, $5, $8)) }
 | ID ASSIGN ID OPAREN argList CPAREN OSQUARED ID CSQUARED OSQUARED CSQUARED {WireAssign($1, ValueOf($3, $5, $8))}

call:
 | ID OPAREN argList CPAREN {}
print: | PRINT ID {}

declare:
 | ID {}

modulez:
 | ID OPAREN argList CPAREN OCURL structureList CCURL {makeModule($3, $6)}
 | comment{}

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
