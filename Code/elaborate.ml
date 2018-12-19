(*Elaborate contains 4 important functions:
        * 1. 
        *)

open Ast
open Printer
module StringMap = Map.Make(String)


(*----------------collapse (the first)-------------*)

type mapList = {n: string; a: binExpr StringMap.t; c: int StringMap.t; net: binExpr list}
(* n (name) is current module's name
 * a (argMap) maps formals to their real arguments
 * c (countMap) says how many times each module name has been called
 * net is the netlist being generated
 *)

let listInvert (lst, b) = List.map (fun a->(a,b)) lst

(* mapList -> binExpr -> (binExpr * mapList):
 * recursively collapses everything inside exp
 * and returns collapsed exp' and global data*)
let rec collapseFn maps exp = match exp with  
    | Buslit(x) -> (Buslit(x), maps)
    | BoolId(x) -> let newX = 
            if (StringMap.mem x maps.a)
                then StringMap.find x maps.a 
                else BoolId(x) in
            let newerX = match newX with
                  BoolId(x) ->
                      if (x.[0] = '.')
                      then BoolId(x)
                      else BoolId ("."^maps.n^"_"^string_of_int (StringMap.find maps.n maps.c)^"_"^x)
                | a -> a in
            (newerX, maps)
    | BoolBinop(lval, op, rval) -> 
            let l2 = collapseFn maps lval in
            let r2 = collapseFn (snd l2) rval in
            (BoolBinop(fst l2, op, fst r2), snd r2)
    | Unop(op, exp) -> 
            let e = collapseFn maps exp in
            (Unop(op, fst e), snd e)
    | Assign(isReg, lval, rval, init) -> 
            let r2 = collapseFn maps  rval in
            let l2 = collapseFn (snd r2) lval in
            (Assign(isReg, fst l2, fst r2, init), snd l2)
    | Index(ModExpr(md, args),
            Range(IntId(a),b)) -> collapseMod maps (ModExpr(md,args)) a
    | Index (exp, rng) ->
            let exp2 = collapseFn maps exp in
            (Index(fst exp2, rng), snd exp2)
    | Print (str, exp) -> 
            let p = collapseFn maps exp in
            (Print(str, fst p), snd p)
    | Call(str, exp) -> 
            print_endline ("Something is wrong! Call called in elaborate");
            (Noexpr, maps)
    | For(str,Range(Lit(a),Lit(b)),exprLst) -> 
             let oldMap = maps in
             let forFn maps expr = 
                  let cex = collapseFn maps expr in
                  let maps' = snd cex in
                  let cexp = fst cex in
                  {n=maps.n; a=maps'.a; c=maps'.c; net=cexp::maps'.net} in
             let maps = {n=maps.n; a=maps.a; c=maps.c; net=[]} in
             let exprLst = List.fold_left forFn maps exprLst in
             let maps = {n=maps.n; a=maps.a; c=exprLst.c; net=oldMap.net} in
             (For(str,Range(Lit(a),Lit(b)),exprLst.net), maps) 
    | ModExpr(md, args) -> collapseMod maps (ModExpr(md,args)) ""
    | Noexpr -> (Noexpr,maps)
    | x -> (Noexpr, maps)

(*This does exactly the same as collapseFn, but since 
 * for Index(ModExpr,_) and ModExpr are nearly identical, it
 * didn't make sense to write each separately.
 * Takes in "a" as the selected elt *)
and collapseMod m (ModExpr(MD(out,nm,fm,exps),args)) ind =
        let oldMap = m in

        (* set m to be identical, except with updated args *)
        let argFn m arg = snd(collapseFn m arg) in
        let m = List.fold_left argFn m args in
        let m = {n=m.n; a=m.a; c=oldMap.c; net=m.net} in

        (* update a for the new args *)
        let aBuilder outmap (sz,nm) arg = StringMap.add nm (fst(collapseFn m arg)) outmap in
        let a = List.fold_left2 aBuilder StringMap.empty fm args in
        let m = {n=nm; a=a; c=m.c; net=m.net} in

        (* update c for this mod*)
        let c =
            if StringMap.mem m.n m.c
            then 
               let count = (StringMap.find m.n m.c)+1 in
               StringMap.add m.n count m.c
            else StringMap.add m.n 0 m.c in
        let m = {n=m.n; a=m.a; c=c; net=m.net} in

        (* collapse every line in current mod, and add each to netlist*)
        let foldFn m exp =
                let collapsedEx = collapseFn m exp in
                let m = snd collapsedEx in
                {n=m.n; a=m.a; c=m.c; net=(fst collapsedEx)::m.net} in
        let m = List.fold_left foldFn m exps in
    
        (* get module's output *)
        let getOut (*By Jordan Peele*) = 
            if (ind = "")
            then 
                if (List.length out >0)
                then collapseFn m (BoolId(snd (List.hd out)))
                else (Noexpr, m)
            else
                let findFn nm (sz,bindName) = nm=bindName in
                collapseFn m (BoolId(snd (List.find (findFn ind) out)))
        in
        let m = {n=oldMap.n; a=oldMap.a; c=m.c; net=m.net} in
        (fst getOut, m)

let collapse ast = 
        let strtMap = {n=""; a=StringMap.empty; c=StringMap.empty; net=[]} in
        List.rev (snd (collapseFn strtMap ast)).net;;


(*-------------------collapse2-------------------*)

let getStrOrLit = function
      Buslit(x) -> x
    | BoolId(x) -> x
    | x -> print_endline("mc-getStrOrlit: "^Printer.getBinExpr "" x); ""

let collapseFn2 = function
    Assign(_,l,r,_) -> (match r with
          BoolBinop(l2,op,r2) ->
            (getStrOrLit(l), opToStr (B(op)), getStrOrLit(l2), getStrOrLit(r2))
        | Unop(op, exp) ->
            (getStrOrLit(l), opToStr (U(op)), getStrOrLit(exp), getStrOrLit(exp))
        | Buslit(x) ->
            (getStrOrLit(l), "Ident", x, x)
        | BoolId(x) -> 
            (getStrOrLit(l), "Ident", x, x)
        | x -> let _ = print_endline("missed case: "^ Printer.getBinExpr "" x)  in
                    (getStrOrLit(l), "", "", "")
    )
  | Print(nm, r) -> (nm, "Print", getStrOrLit(r), getStrOrLit(r))
  | a -> print_endline ("something else!!"); 
         print_endline (Printer.getBinExpr "" a);
         ("","","","")

let collapse2 (prenet, globs) = 
    (List.map collapseFn2 prenet, globs)


(*REGISTERS*)
let fst (a,_,_) = a
let snd (_,b,_) = b

let regFn (netout, globs, count) x = match x with 
       Assign(true,BoolId(lval),rval,init) -> 
            let x = Assign(true,BoolId("&u_"^ sOfI count),rval,init) in
            let g = (lval, "&u_"^ sOfI count, init) in
            (x::netout, g::globs, count+1)
     | x -> (x::netout, globs, count) 

let regs netlist =
    let lst = List.fold_left regFn ([],[],0) netlist in
    (List.rev (fst lst), snd lst) 

(*ADD REGASSIGNS AT BOTTOM*)

let r2 (netlist, globs) =
        let assFn (l,r,_) = (l,"Ident",r,r) in
        let assgs = List.map assFn globs in
        let fn outlist expr = expr::outlist in  
        let netlist = List.rev (List.fold_left fn (List.rev netlist) assgs) in
        (netlist, globs)
