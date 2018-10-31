type intOp = Add | Sub
type boolOp = And | Or | Nand | Nor | Xor | Xnor
type unOp = Not
type typ = Int | Bus

type boolval = 
        One | Zero

type bind = typ * intExpr * string

and intExpr = 
      Lit of int
    | IntId of string
    | IntBinop of intExpr * intOp * intExpr

and  binExpr = 
      Buslit of string (*since lex is annoying about turning 01011 into [0;1;0;1;1] keep as string until later down the line*)
    | BoolId of string
    | BoolBinop of binExpr * boolOp * binExpr
    | Unop of unOp * binExpr
    | Assign of bool * string * range * binExpr * bool
    (*when bool = false, normal; bool = true, register*)
    (*final bool is init state*)
    (*add new assign indexing rule to LRM*)
    | Call of string * binExpr list * range
    | For of string * range * binExpr list
    | Noexpr

and range = Range of intExpr * intExpr

type md = Module_decl of bind list * string * bind list * binExpr list

type program = bind list * md list
