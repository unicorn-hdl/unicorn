open Ast
open Printer
module StringMap = Map.Make(String)

let nl = []
(*(* lookup table for finding variables *)
let lookup n = try StringMap.find n local_vars
				with Not_Found -> raise (Failure ("unrecognized function " ^ n))
in

let _ = find_func "main" in (* Ensure "main" is defined *)
in
(* Build local symbol table of variables for this function *)
let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	            StringMap.empty (globals @ func.formals @ func.locals )
in

let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;
*)
(* expression builder: looks at expressions and tells compiler what to make that into for LLVM to understand 
first match sees integers and tells LLVM it is a 32 bit val int with value i *)
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


let callx x = ( Call(x, []) )
let modA = Module_decl([], "modA", [], [])
let bExprs = 
        [Index(Buslit("00100"), Range(IntBinop((Lit(4)), Sub, Lit(1)), Lit(3)))]
let modB = Module_decl([], "modB", [], bExprs)
let mdlistEx = [modA;
                modB
               ]

let toString (Module_decl(a,b,c,d)) = b ^ "\n" ^ toStringBinExprlist d;;
List.iter (fun x -> print_endline (toString x)) (harden mdlistEx)
