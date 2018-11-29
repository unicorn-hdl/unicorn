
type intOp = Add | Sub
type boolOp = And | Or | Nand | Nor | Xor | Xnor
type unOp = Not
type boolval = One of bool | Zero of bool

type typ = Int | Bus of intExpr

and bind = intExpr * string

and intExpr = 
      Lit of int
    | IntId of string
    | IntBinop of intExpr * intOp * intExpr

and  binExpr = 
      Buslit of string (*since lex is annoying about turning 01011 into [0;1;0;1;1] keep as string until later down the line*)
    | BoolId of string
    | BoolBinop of binExpr * boolOp * binExpr
    | Unop of unOp * binExpr
    | Assign of bool * binExpr *  binExpr * bool
    | Index of binExpr * range
    (*when bool = false, normal; bool = true, register*)
    (*final bool is init state*)
    (*add new assign indexing rule to LRM*)
    | Print of string * binExpr
    | Call of string * binExpr list
    | For of string * range * binExpr list
    | ModExpr of md * binExpr list * md 
    | Noexpr

and range = Range of intExpr * intExpr

and md = Module_decl of bind list * string * bind list * binExpr list
                        (*outlist   name      formals     line list *)  

type program = (*bind list *  *) md list
