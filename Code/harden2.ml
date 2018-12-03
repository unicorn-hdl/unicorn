open Ast
open Printer
module StringMap = Map.Make(String)

exception UndeclaredVar of string 

let modzIntoTuples d m = List.map (fun d-> (d,m)) d
let bindsToMaps binds m = List.map (fun (a,b)->(a,b,m)) binds

(* make a table for varnames -------------------------------*)

let inline lines = StringMap.empty
let lookup args = StringMap.empty

(*have to make actual functions here*)
let conflict1 key val1 val2 = Some val1 
let conflict2 key val1 val2 = Some val1 
(*----------------------------------*)

let valsInMod (Module_decl(outs, name, formals, exprs)) = 
        (name, (StringMap.union conflict2 (StringMap.union conflict1 (lookup outs) (lookup formals)) (inline exprs)))
(*end-------------------------------------------------------*)

(*Harden ---------------------------------------------------*)
let extract (Lit(x)) = x

let rec eval expr nameMap = match expr with
	 | Lit(x) -> Lit(x)
	 | IntBinop(lval, op, rval) -> if op = Add 
	 		then Lit(extract (eval lval nameMap) + extract (eval rval nameMap))
			else Lit(extract (eval lval nameMap) - extract (eval rval nameMap))
	 | IntId(name) -> if StringMap.mem name nameMap 
            then Lit(StringMap.find name nameMap)
            else raise (UndeclaredVar ("Variable \"" ^ name ^ "\" is not defined!"))

let rec hardenline (line, nameMap) = match line with
	| Index(expr, Range(a,b)) -> Index(expr, Range(eval a nameMap, eval b nameMap))
	| For(index, Range(a,b), exprs) -> For(
                index, 
                Range(eval a nameMap, eval b nameMap), 
                List.map hardenline (modzIntoTuples exprs nameMap))
	| a -> a

let hardenarg (expr, name, nameMap) = (eval expr nameMap, name)

let hardenmd (Module_decl(outs, name, formals, linelist)) =
    let table = StringMap.empty (*TODO: must generate table*) in
	Module_decl(List.map hardenarg (bindsToMaps outs table),
                name,
                List.map hardenarg (bindsToMaps formals table),
                List.map hardenline (modzIntoTuples linelist table))
(*end-------------------------------------------------------*)


(*THE function called---------------------------------------*)
let harden ast = hardenmd ast
(*end-------------------------------------------------------*)

(*This is just for printing --------------------------------*)

(*build an ast*)
let callx x = ( Call(x, []) );;
let modA = Module_decl([], "modA", [], []);;
let bExprs = 
        [Index(Buslit("00100"), Range(IntBinop((Lit(4)), Sub, Lit(1)), Lit(3)));
        Print("printname", callx "modA")];;
let modB = Module_decl([], "modB", [], bExprs);;
let mdlistEx = [modA;
                modB
               ]
;;
(*
let mdlistEx = fill mdlistEx;;
(*------------*)

(*print hardened ast----------------------------------------*)
print_endline("~~~PRINTING HAST~~~");;
let toString (Module_decl(a,b,c,d)) = b ^ "\n" ^ toStringBinExprlist d;;
print_endline (toString (harden mdlistEx));;
(*----------------------------------------------------------*)

(*print val table*)
print_endline ("\n~~~PRINTING VAL TABLE~~~");;
let valtable = valsInMod mdlistEx;; 
let printfun key v = print_string(key ^ ": ");
                     List.iter (fun (a,v)->print_string(string_of_int v)) v;
                     print_endline "";;
(*
StringMap.iter printfun valtable
*)
(*---------------*)
*)
