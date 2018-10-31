type intOp = Add | Sub
type boolOp = And | Or | Nand | Nor | Xor | Xnor
type unOp = Not
type typ = Int | Bus
type bind = typ * string

type boolval = 
        One | Zero

type intExpr = 
      Literal of int
    | IntId of string
    | IntBinop of intExpr * intOp * intExpr
    | Max of int

and  binExpr = 
      Buslit of bool list
    | BoolId of string
    | BoolBinop of binExpr * boolOp * binExpr
    | Unop of unOp * binExpr
    | Assign of bool * string * intExpr * intExpr * binExpr * bool
    (*when bool = false, normal; bool = true, register*)
    (*add new assign indexing rule to LRM*)
    | Call of string * binExpr list
    | For of string * intExpr * intExpr
    | Noexpr

type module_decl = { typ    : int list;
					mname   : string;
					formals : bind list;
					locals  : bind list;
					body    : binExpr list; }

type program = bind list * module_decl list
