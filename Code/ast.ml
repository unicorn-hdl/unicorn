type intOp = Add | Sub
type boolOp = And | Or | Nand | Nor | Xor | Xnor
type unOp = Not | Ident
type boolval = One of bool | Zero of bool
type genOp = B of boolOp | U of unOp

type typ = Int | Bus of intExpr

and bind = intExpr * string

and intExpr = 
      Lit of int
    | IntId of string
    | IntBinop of intExpr * intOp * intExpr

and  binExpr = 
      Buslit of string 
    | BoolId of string
    | BoolBinop of binExpr * boolOp * binExpr
    | Unop of unOp * binExpr
    | Assign of bool * binExpr * binExpr * string 
    | Index of binExpr * range
    | Print of string * binExpr
    | Call of string * binExpr list
    | For of string * range * binExpr list
    | ModExpr of md * binExpr list
    | Noexpr

and range = Range of intExpr * intExpr

and md = MD of bind list * string * bind list * binExpr list
                        (*outlist   name      formals     line list *)  

type program = md list

let opToStr = function
      B(x) -> (match x with
            And  -> "And"
          | Or   -> "Or"
          | Nand -> "Nand"
          | Nor  -> "Nor"
          | Xor  -> "Xor"
          | Xnor -> "Xnor" 
      )
    | U(x) -> (match x with
          | Not -> "Not" 
          | Ident -> "Ident")
