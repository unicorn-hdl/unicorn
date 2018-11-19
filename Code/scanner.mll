(*Ocamllex scanner for UNI-corn*)

{ open Parser }
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 87c88e7be77c68e126af93f5cf71f9e93d45daa9
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
<<<<<<< HEAD
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
=======
>>>>>>> 87c88e7be77c68e126af93f5cf71f9e93d45daa9
| '{' { OCURL }
| '}' { CCURL }
| '[' { OSQUARED }
| ']' { CSQUARED }
| '(' { OPAREN }
| ')' { CPAREN }
| ';' { SEMI }
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 87c88e7be77c68e126af93f5cf71f9e93d45daa9
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
<<<<<<< HEAD
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
| "print" { PRINT }
| '1' { ONE }
| var as lxm { ID(lxm) }
| int as lxm  { LITERAL(int_of_string lxm) } (*does this need 'as lxm'*)
| '0' { ZERO }
| eof { EOF }
|"🦄" { UNICORN }
| _ as ch { raise (Failure("illegal character " ^ Char.escaped ch)) }
and multi = parse
<<<<<<< HEAD
    '**/' { token lexbuf }
>>>>>>> 3babc15... scanner 1.0
=======
    "**/" { token lexbuf }
>>>>>>> 1412556... changed ' to " for tokens that were strings
=======
>>>>>>> 87c88e7be77c68e126af93f5cf71f9e93d45daa9
|   _     { multi lexbuf }

and single = parse
    '\n' { token lexbuf }
|   _    { single lexbuf }

