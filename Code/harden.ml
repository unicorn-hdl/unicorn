
module A = Ast
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
let rec hardenline = function
	| Index(expr, Range(a,b)) -> Index(expr, Range(eval a, eval b))
	| For (index, Range(a,b), exprs) -> For(index, Range(eval a, eval b), hardenline exprs)
	| a -> a

let rec eval = function
	 | Lit(x) -> x
	 | IntBinop(lval, op, rval) -> if op = Add 
	 				then eval lval + eval rval 
					else eval lval - eval rval
	 | IntId(name) -> 1 (*this must be done*)

let hardenarg (expr, name) = (eval expr, name)

let hardenmd Module_decl(outs, name, formals, linelist) =
	Module_decl(List.map hardenarg outs, name, List.map hardenarg formals, List.map hardenline linelist)

let harden ast = 
	List.map hardenmd ast
