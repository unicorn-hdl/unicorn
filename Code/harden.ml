open Ast
open Printer
module StringMap = Map.Make(String)

let modzIntoTuples d m = List.map (fun d-> (d,m)) d

(* make a table for which modules call a given module*)

let rec isACall ls = function
    | Buslit(x) -> ls
    | BoolId(x) -> ls
    | BoolBinop(l, op, r) -> isACall ls l @ ls @ isACall ls r
    | Unop(op, exp) -> isACall ls exp @ ls
    | Assign(a, e1, e2, b) -> isACall ls e1 @ ls @ isACall ls e2
    | Index(e, r) -> isACall ls e @ ls
    | Print(s, e) -> isACall ls e @ ls
    | For(s, r, e) -> getlines e @ ls 
    | ModExpr(Module_decl(o,name,f,exprs)) -> if List.mem name ls 
                        then ls 
                        else name :: getlines exprs @  ls
    | Noexpr -> ls 
    | _ -> ls
(*
and isACallOnTuples (exprs, ls) = isACall ls exprs
and runThroughLines ls exprs = List.fold_left isACallOnTuples (modzIntoTuples exprs ls)
this is similar to get lines, but more complicated*)
and getlines lines = List.fold_left isACall [] lines

let buildTable m (Module_decl(out, name, formal, lines)) = StringMap.add name (getlines lines) m
let table mdlist = List.fold_left buildTable StringMap.empty mdlist

(*--------------------------------------------------*)

let extract (Lit(x)) = x

let rec eval = function
	 | Lit(x) -> Lit(x)
	 | IntBinop(lval, op, rval) -> if op = Add 
	 				then Lit(extract (eval lval) + extract (eval rval))
					else Lit(extract (eval lval) - extract (eval rval))
	 | IntId(name) -> Lit(1) (*this must be done*)

let rec hardenline = function
	| Index(expr, Range(a,b)) -> Index(expr, Range(eval a, eval b))
	| For(index, Range(a,b), exprs) -> For(index, Range(eval a, eval b), List.map hardenline exprs)
	| a -> a

let hardenarg (expr, name) = (eval expr, name)

let hardenmd (Module_decl(outs, name, formals, linelist)) =
	Module_decl(List.map hardenarg outs, name, List.map hardenarg formals, List.map hardenline linelist)

let harden ast = 
	List.map hardenmd ast

(*This is just for printing ---------------------------*)
let callx x = ( Call(x, []) )
let modA = Module_decl([], "modA", [], [])
let bExprs = 
        [Index(Buslit("00100"), Range(IntBinop((Lit(4)), Sub, Lit(1)), Lit(3)));
        Print("printname", callx "modA")]
let modB = Module_decl([], "modB", [], bExprs)
let mdlistEx = [modA;
                modB
               ]

let toString (Module_decl(a,b,c,d)) = b ^ "\n" ^ toStringBinExprlist d;;
List.iter (fun x -> print_endline (toString x)) (harden mdlistEx);;
let printfun key v = print_string(key ^ ": ");
                     print_string (string_of_int(List.length v));
                     List.iter print_string v; print_endline "";;
print_endline("table values");;
StringMap.iter printfun (table mdlistEx)
