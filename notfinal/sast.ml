open Ast

type hardBind = int * string
type hardmd = {outlistH: hardBind list;
               nameH: string;
               formalsH: hardBind list;
               localsH: hardBind list;
               linelistH: binExpr list}

type sintExpr  = int * sintExpr2
and  sintExpr2 = 
      SLit of int
    | SIntId of string
    | SIntBinop of sintExpr * intOp * sintExpr

type sbinExpr  = int * sbinExpr2
and  sbinExpr2 = 
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

(*Important: Ast does not contatin a "locals" bind list, but this info is contained in the lineList. It must be extracted and put in the locals list here, when semant is building smds*)
type smd = {outlist: hardBind list;
            name: string;
            formals: hardBind list;
            linelist: sbinExpr list}

type sprogram = (*bind list *  *) smd list

let string_of_sprogram = function
| _ -> "this"
