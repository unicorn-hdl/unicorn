open Ast
(*
type intOp = Add | Sub
type boolOp = And | Or | Nand | Nor | Xor | Xnor
type unOp = Not
type typ = Int | Bus

type boolval = 
        One of bool | Zero of bool
*)

(* I wasn't sure how to handle the bind*)
type bind = typ * intExpr * string

and intExpr = 
      SLit of int
    | SIntId of string
    | SIntBinop of sintExpr * intOp * sintExpr

and  sbinExpr = 
      SBuslit of string (*since lex is annoying about turning 01011 into [0;1;0;1;1] keep as string until later down the line*)
    | SBoolId of string
    | SBoolBinop of sbinExpr * boolOp * sbinExpr
    | SUnop of unOp * binExpr
    | Assign of bool * binExpr *  binExpr * bool
    | Index of binExpr * range
    (*when bool = false, normal; bool = true, register*)
    (*final bool is init state*)
    (*add new assign indexing rule to LRM*)
    | Print of string * binExpr
    | Call of string * binExpr list
    | For of string * range * binExpr list
    | Noexpr

and range = Range of intExpr * intExpr

type md = Module_decl of bind list * string * bind list * binExpr list
                        (*outlist   name      formals     line list *)  

type program = (*bind list *  *) md list

let string_of_program = function
| _ -> "this"
