(*Ocamllex scanner for UNI-corn*)

{ open Parser }

let digit = ['0'-'9']
let int = digit+
let alphaNum = ['A'-'z' '0'-'9']
let alphaLow = ['a'-'z']
let var = (alphaLow)(alphaNum)*
let whitespace = [' ' '\r' '\t' '\n']

rule token = parse
    whitespace { token lexbuf }
| '//' { single lexbuf }
| '/**' { multi lexbuf }
| '{' { OCURL }
| '}' { CCURL }
| '[' { OSQUARED }
| ']' { CSQUARED }
| '(' { OPAREN }
| ')' { CPAREN }
| ';' { SEMI }
| ',' { COMMA }
| '=' { ASSIGN }
| 'out' { OUT } 
| '<' { OGENERIC }
| '>' { CGENERIC }
| ':=' { REGASSIGN }
| '+' { PLUS }
| '-' { MINUS }
| 'for' { FOR }
| 'to' { TO }
| 'from' { FROM }
| 'init' { INIT }
| 'AND' { AND }
| 'OR' { OR }
| 'NOT' { NOT }
| 'NAND' { NAND }
| 'NOR' { NOR }
| 'XOR' { XOR }
| 'XNOR' { XNOR }
| 'module' { MODULE }
| 'main' { MAIN }
| 'print' { PRINT }
| '1' { ONE }
| var { ID(var) }
| int { LITERAL(int_of_string int) } (*does this need 'as lxm'*)
| '0' { ZER0 }
| eof { EOF }
|'🦄' { UNICORN }
| _         { error lexbuf (* do we need this? *)
                "found '%s' - don't know how to handle" @@ get lexbuf }
and multi = parse
    '**/' { token lexbuf }
|   _     { multi lexbuf }

and single = parse
    '\n' { token lexbuf }
|   _    { single lexbuf }

