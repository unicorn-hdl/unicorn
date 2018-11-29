open Ast
open Printer
module StringMap = Map.Make(String)

let modzIntoTuples d m = List.map (fun d-> (d,m)) d

(* make a table for which modules call a given module-------*)
let rec isACall ls = function
    | Buslit(x) -> ls
    | BoolId(x) -> ls
    | BoolBinop(l, op, r) -> isACall ls l @ ls @ isACall ls r
    | Unop(op, exp) -> isACall ls exp @ ls
    | Assign(a, e1, e2, b) -> isACall ls e1 @ ls @ isACall ls e2
    | Index(e, r) -> isACall ls e @ ls
    | Print(s, e) -> isACall ls e @ ls
    | For(s, r, e) -> getlines e @ ls 
    | ModExpr(Module_decl(o,name,f,exprs), args, par) -> if List.mem name ls 
                        then ls 
                        else name :: getlines exprs @  ls
    | Noexpr -> ls 
    | _ -> ls
and getlines lines = List.fold_left isACall [] lines

let buildTable m (Module_decl(out, name, formal, lines)) = StringMap.add name (getlines lines) m
let makeModTable mdlist = List.fold_left buildTable StringMap.empty mdlist
(*end-------------------------------------------------------*)

(* make a table for varnames -------------------------------*)

let inline lines = StringMap.empty
let lookup args = StringMap.empty

(*have to make actual functions here*)
let conflict1 key val1 val2 = Some val1 
let conflict2 key val1 val2 = Some val1 
(*----------------------------------*)

let valsInMod (Module_decl(outs, name, formals, exprs), modTable) = 
        (name, (StringMap.union conflict2 (StringMap.union conflict1 (lookup outs) (lookup formals)) (inline exprs)))
let makeValTables mdlist modTable = List.map valsInMod (modzIntoTuples mdlist modTable) 
(*end-------------------------------------------------------*)

(*Harden ---------------------------------------------------*)
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
(*end-------------------------------------------------------*)


(*THE function called---------------------------------------*)
let harden ast = 
    let modTable = makeModTable ast in
    let valTable = makeValTables ast modTable in
    let hast = List.map hardenmd ast in 
    hast
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
let mdlistEx = [fill mdlistEx];;
(*------------*)

(*print hardened ast----------------------------------------*)
print_endline("~~~PRINTING HAST~~~");;
let toString (Module_decl(a,b,c,d)) = b ^ "\n" ^ toStringBinExprlist d;;
List.iter (fun x -> print_endline (toString x) ) (harden mdlistEx);;
(*----------------------------------------------------------*)

(* print mod table------------------------------------------*)
print_endline("~~~PRINTING MOD REF TABLE~~~");;
let printfun key v = print_string(key ^ ": ");
                     List.iter print_string v;
                     print_endline "";;
StringMap.iter printfun (makeModTable mdlistEx);;
(*end-------------------------------------------------------*)

(*print val table*)
print_endline ("\n~~~PRINTING VAL TABLE~~~");;
let valtable = makeValTables mdlistEx (makeModTable mdlistEx);; 
let printfun key v = print_string(key ^ ": ");
                     List.iter (fun (a,v)->print_string(string_of_int v)) v;
                     print_endline "";;
(*
StringMap.iter printfun valtable
*)
(*---------------*)
