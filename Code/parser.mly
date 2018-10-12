/* Ocamlyacc parser for UNI-corn*/

%{
open Ast 
%}

%token OPAREN CPAREN OCURL CCURL OSQUARED CSQUARED 
%token COMMA
%token SEMI UNICORN EOF
%token OGENERIC CGENERIC 
%token ASSIGN REGASSIGN 
%token LINECOMMENT OBLOCK CBLOCK
%token PLUS MINUS
%token FOR TO FROM
%token OUT INIT
%token AND OR NOT NAND NOR XOR XNOR
%token MODULE 
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
 | mainz modulez UNICORN EOF {}
 | mainz EOF {}

 mainz:
 | MAIN OPAREN argList CPAREN OCURL body CCURL {}
 
 body:
 | structureList{} 

 structureList:
 | /* Nothing */ {}
 | structure structureList {}

 structure:
 | line  {}
 | loop {}

 argList:
 | /*Nothing*/ {}
 | argList COMMA arg {}
 
 arg:
 | ID{}
 | ID OSQUARED LITERAL CSQUARED {}
 | ID OGENERIC ID OGENERIC {}

 line: 
 | assignment SEMI {}
 | call  SEMI {} 
 | print SEMI {} 
 | declare SEMI {}

 boolval:
 | ONE {}
 | ZERO {}

 assignment:
 | ID REGASSIGN ID INIT boolval{}
 | ID ASSIGN ID OPAREN argList CPAREN {}
 | ID ASSIGN ID OPAREN argList CPAREN OSQUARED ID CSQUARED {}
 | ID ASSIGN ID OPAREN argList CPAREN OSQUARED ID CSQUARED OSQUARED CSQUARED {}

 call:
 | ID OPAREN argList CPAREN {}

 print:
 | PRINT ID {}

 declare:
 | ID {}

 modulez:
 | MODULE ID OPAREN argList CPAREN OCURL body CCURL {}

 varNum:
 | ID {}
 | LITERAL {}

 loop:
 | FOR OPAREN ID FROM varNum TO varNum CPAREN OCURL loopBody CCURL {}
 | FOR OPAREN ID TO varNum CPAREN OCURL loopBody CCURL {}

 loopBody:
 | structure {}
