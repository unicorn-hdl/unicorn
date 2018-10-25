type intOp = Add | Sub
type boolOp = And | Or | Nand | Nor | Xor | Xnor
type unOp = Not
type typ = Int | Bus
type bind = typ * string

type intExpr = 
      Literal of int
    | Id of string
    | IntBinop of intExpr * intOp * intExpr

and type binExpr = 
      Buslit of bool list
    | Id of string
    | BoolBinop of binExpr * binOp * binExpr | Unop of unOp * binExpr
    | Assign of string * binExpr
    | Call of string * binExpr list
    | For of Id * intExpr * intExpr
    | Noexpr

type module_decl = { typ    : int list;
					mname   : string;
					formals : bind list;
					locals  : bind list;
					body    : binExpr list; }

type program = bind list * module_decl list
