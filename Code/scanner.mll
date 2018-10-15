(*Ocamllex scanner for UNI-corn*)

{ open Parser }
<<<<<<< HEAD
let digit = ['0'-'9']
let int = digit+
let alphaNum = ['A'-'Z' 'a'-'z' '0'-'9']
let alphaLow = ['A'-'Z' 'a'-'z']
let var = (alphaLow)(alphaNum)*
let whitespace = [' ' '\r' '\t' '\n']
let boollist = ('0'|'1')+ ('b')
rule token = parse
    whitespace { token lexbuf }
| "//" { single lexbuf }
| "/**" { multi lexbuf }
=======

let digit = ['0'-'9']
let int = digit+
let alphaNum = ['A'-'z' '0'-'9']
let alphaLow = ['a'-'z']
let var = (alphaLow)(alphaNum)*
let whitespace = [' ' '\r' '\t' '\n']

rule token = parse
    whitespace { token lexbuf }
<<<<<<< HEAD
| '//' { single lexbuf }
| '/**' { multi lexbuf }
>>>>>>> 3babc15... scanner 1.0
=======
| "//" { single lexbuf }
| "/**" { multi lexbuf }
>>>>>>> 1412556... changed ' to " for tokens that were strings
| '{' { OCURL }
| '}' { CCURL }
| '[' { OSQUARED }
| ']' { CSQUARED }
| '(' { OPAREN }
| ')' { CPAREN }
| ';' { SEMI }
<<<<<<< HEAD
| ':' { COLON }
| ',' { COMMA }
| '=' { ASSIGN }
| "out" { OUT } 
| '<' { OGENERIC }
| '>' { CGENERIC }
| ":=" { REGASSIGN }
| '+' { PLUS }
| '-' { MINUS }
| "for" { FOR }
| "to" { TO } | "from" { FROM }
| "init" { INIT }
| "and" { AND }
| "or" { OR }
| "not" { NOT }
| "nand" { NAND }
| "nor" { NOR }
| "xor" { XOR }
| "xnor" { XNOR }
| "print" { PRINT }
| "make" { MAKE }
| "*0*" { ZERO }
| "*1*" { ONE } 
| var as lxm { ID(lxm) }
| int as lxm  { LITERAL(int_of_string lxm) } (*does this need 'as lxm'*)
| boollist as lxm { BOOLLIT(lxm) }
| eof { EOF }
|"🦄" { UNICORN }
| "neigh" { UNICORN2}
| _ as ch { raise (Failure("illegal character " ^ Char.escaped ch)) }
and multi = parse
    "**/" { token lexbuf }
=======
| ',' { COMMA }
| '=' { ASSIGN }
| "out" { OUT } 
| '<' { OGENERIC }
| '>' { CGENERIC }
| ":=" { REGASSIGN }
| '+' { PLUS }
| '-' { MINUS }
| "for" { FOR }
| "to" { TO }
| "from" { FROM }
| "init" { INIT }
| "and" { AND }
| "or" { OR }
| "not" { NOT }
| "nand" { NAND }
| "nor" { NOR }
| "xor" { XOR }
| "xnor" { XNOR }
| "main" { MAIN }
| "print" { PRINT }
| '1' { ONE }
| var { ID(var) }
| int { LITERAL(int_of_string int) } (*does this need 'as lxm'*)
| '0' { ZER0 }
| eof { EOF }
|"🦄" { UNICORN }
| _         { error lexbuf
                "found '%s' - don't know how to handle" @@ get lexbuf }
and multi = parse
<<<<<<< HEAD
    '**/' { token lexbuf }
>>>>>>> 3babc15... scanner 1.0
=======
    "**/" { token lexbuf }
>>>>>>> 1412556... changed ' to " for tokens that were strings
|   _     { multi lexbuf }

and single = parse
    '\n' { token lexbuf }
|   _    { single lexbuf }

