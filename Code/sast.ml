open Ast

type bind = typ * intExpr * string

and sintExpr = 
      SLit of int
    | SIntId of string
    | SIntBinop of sintExpr * intOp * sintExpr

and  sbinExpr = 
      SBuslit of string (*since lex is annoying about turning 01011 into [0;1;0;1;1] keep as string until later down the line*)
    | SBoolId of string
    | SBoolBinop of sbinExpr * boolOp * sbinExpr
    | SUnop of unOp * sbinExpr
    | SAssign of bool * sbinExpr * sbinExpr * bool
    | SIndex of sbinExpr * srange
    (*when bool = false, normal; bool = true, register*)
    (*final bool is init state*)
    (*add new assign indexing rule to LRM*)
    | SPrint of string * sbinExpr
    | SCall of string * sbinExpr list
    | SFor of string * srange * sbinExpr list
    | SNoexpr

and srange = SRange of sintExpr * sintExpr

type smd = SModule_decl of bind list * string * bind list * sbinExpr list
                        (*outlist   name      formals     line list *)  

type sprogram = (*bind list *  *) smd list

let string_of_sprogram = function
| _ -> "this"
