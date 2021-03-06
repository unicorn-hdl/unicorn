(*Ocamllex scanner for UNI-corn*)

{ open Parser }
let digit = ['0'-'9']
let int = digit+
let alphaNum = ['A'-'Z' 'a'-'z' '0'-'9' '\'']
let alphaLow = ['A'-'Z' 'a'-'z']
let var = (alphaLow)(alphaNum)*
let whitespace = [' ' '\r' '\t' '\n']
let boollist = ('0'|'1')+ ('b')
rule token = parse
    whitespace { token lexbuf }
| "//" { single lexbuf }
| "/**" { multi lexbuf }
| '{' { OCURL }
| '}' { CCURL }
| '[' { OSQUARED }
| ']' { CSQUARED }
| '(' { OPAREN }
| ')' { CPAREN }
| ';' { SEMI }
| ':' { COLON }
| ',' { COMMA }
| '=' { ASSIGN }
| '<' { OGENERIC }
| '>' { CGENERIC }
| ":="{ REGASSIGN }
| "*" { STAR  } 
| "!*"{ EXCITEDSTAR }
| "#" { POUND }
| "!#"{ EXCITEDPOUND }
| '+' { PLUS  }
| "!+"{ EXCITEDPLUS  }
| "!" { WOW }
| '-' { MINUS }
| "out" { OUT } 
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
| var as lxm { ID(lxm) }
| int as lxm  { LITERAL(int_of_string lxm) } (*does this need 'as lxm'*)
| boollist as lxm { BOOLLIT(lxm) }
| eof { EOF }
| "🦄" { UNICORN }
| "neigh!" { UNICORN2 }
| _ as ch { raise (Failure("illegal character " ^ Char.escaped ch)) }
and multi = parse
    "**/" { token lexbuf }
|   _     { multi lexbuf }

and single = parse
    '\n' { token lexbuf }
|   _    { single lexbuf }
