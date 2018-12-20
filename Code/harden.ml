(*After being largely cannibalized by semant, harden simply 
 * contains some helper methods for semant*)

open Ast
open Printer
module StringMap = Map.Make(String)

exception UndeclaredVar of string 

let extract (Lit(x)) = x

let rec eval nameMap expr = match expr with
	 | Lit(x) -> Lit(x)
	 | IntBinop(lval, op, rval) -> if op = Add 
	 		then Lit(extract (eval nameMap lval) + extract (eval nameMap rval))
			else Lit(extract (eval nameMap lval) - extract (eval nameMap rval))
	 | IntId(name) -> 
            let name = "*"^name in
            if StringMap.mem name nameMap 
            then Lit(StringMap.find name nameMap)
            else raise (UndeclaredVar ("Variable \"" ^ name ^ "\" is not defined!!"))

let hardenline m line = 
    match line with
    | Index(ModExpr(_,_),_) -> line
	| Index(expr, Range(a,b)) -> 
            Index(expr, Range(eval m a, eval m b))
	| For(index, Range(a,b), exprs) -> 
            let r = Range(eval m a, eval m b) in
            For(index, r, exprs)
	| _ -> line
