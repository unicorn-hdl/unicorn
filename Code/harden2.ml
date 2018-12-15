open Ast
open Printer
module StringMap = Map.Make(String)

exception UndeclaredVar of string 

let extract (Lit(x)) = x

(*
let tableFn inmap outmap (fmX,nm) arg = match fmX with
     | IntId(x) -> 
            let sz = Semant.size outmap arg in
            let outmap = snd sz in
            let nm = "*"^nm in
            if StringMap.mem nm outmap
            then if (StringMap.find nm outmap = fst sz)
                then outmap
                else let _ = p ("error") in outmap
            else StringMap.add nm (fst sz) outmap
     | x -> outmap
*)

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
(*
let rec hardenline m line = 
    match line with
    | Buslit(x) -> line
    | BoolId(x) -> line
    | BoolBinop(l,op,r) ->
            let r = hardenline m r in 
            let l = hardenline m l in
            BoolBinop(l,op,r)
    | Unop(op,x) -> 
            Unop(op, hardenline m x)
    | Assign(isR, l, r, init) ->
            let r = hardenline m r in
            let l = hardenline m l in
            Assign(isR, l, r, init)
	| Index(expr, Range(a,b)) -> 
            let expr = hardenline m expr in
            Index(expr, Range(eval m a, eval m b))
    | Print(nm,x) -> 
            let x = hardenline m x in
            Print(nm, x)
    | Call(_,_) -> p ("Call in harden"); (m, Noexpr)
	| For(index, Range(a,b), exprs) -> 
            let exprs = List.map (hardenline m) exprs  in
            let r = Range(eval m a, eval m b) in
            For(index, r, exprs)
    | ModExpr( MD(out,nm,fm,lns), args, par) -> 
            let foldFn (intEx, nm) = (eval m intEx, nm) 
            let args = List.map foldFn args 
            ModExpr( MD(out,nm,fm,lns), args, par) 
	| a -> a
    *)
let hardenline m line = 
    match line with
    | Index(ModExpr(_,_),_) -> line
	| Index(expr, Range(a,b)) -> 
            Index(expr, Range(eval m a, eval m b))
	| For(index, Range(a,b), exprs) -> 
            let r = Range(eval m a, eval m b) in
            For(index, r, exprs)
	| _ -> line

    (*
let hardenMd m = function
    ModExpr(MD(out, nm, fm, lns), args, par) -> 
        let modMap = List.fold_left2 tableFn m fm args in

        let foldFn ((map,prev)::tl) ln = (hardenline map ln)::(map,prev)::tl in
        let lns = List.fold_left foldFn [(modMap,Noexpr)] lns in

        let mapFn (mp,ex) = ex in
        let lns = List.map mapFn lns in

        let mapFn (intEx,nm) = (eval modMap intEx,nm) in
        let out  = List.map mapFn out in

        (m, ModExpr(MD(out,nm,fm,lns),args,par))
*)
(*end of harden md*)
(*
let rec check m line = 
   match line with
   | Buslit(x) -> (m, line)
   | BoolId(x) -> (m, line)
   | BoolBinop(l,op,r) -> 
         let l = hardenline m l in
         let r = hardenline m r  in
         
   | Unop(op,x) ->
   | Assign(isR,l,r,init) ->
   | Index(x,r) ->
   | Print(nm,x) ->
   | Call(nm,x) ->
   | For(n,r,xs) ->
   | ModExpr(md,args,par) ->
   | Noexpr ->
*)
