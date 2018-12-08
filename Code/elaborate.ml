open Ast
module StringMap = Map.Make(String)

(*Maps should be (n = thisMod'sName, argMap = varsInThisMod, cMap = howManyTimesHasModNameBeenCollapsed, net = netlist)*)
type mapList = {n: string; argMap: binExpr StringMap.t; cMap: int StringMap.t; net: binExpr list}

let listInvert (lst, b) = List.map (fun a->(a,b)) lst


let rec collapseFn maps exp = match exp with  
    | Buslit(x) -> (Buslit(x), maps)
    | BoolId(x) -> let newX = 
                    if (StringMap.mem x maps.argMap)
                        then StringMap.find x maps.argMap 
                        else BoolId(x) in
                    let newerX = match newX with
                         BoolId(x) ->
                            if (x.[0] = '.')
                            then BoolId(x)
                            else BoolId ("."^maps.n ^ "_" ^ string_of_int (StringMap.find maps.n maps.cMap) ^ "_" ^ x)
                       | a -> a in
                    (newerX, maps)
    | BoolBinop(lval, op, rval) -> 
            let l2 = collapseFn maps lval in
            let r2 = collapseFn (snd l2) rval in
            (BoolBinop(fst l2, op, fst r2), snd r2)
    | Unop(op, exp) -> 
            let e = collapseFn maps exp in
            (Unop(op, fst e), snd e)
(*Remember not to collapse on lval of assign. If lval collides with an arg, remove arg from list*)
    | Assign(isReg, lval, rval, init) -> 
            let r2 = collapseFn maps  rval in
            let l2 = collapseFn (snd r2) lval in
            (Assign(isReg, fst l2, fst r2, init), snd l2)
    | Index(ModExpr(Module_decl(out,nm,fm,exps), args, par),Range(IntId(a),b)) ->
        let oldMap = maps in
        let argMapBuilder map (sz,nm) arg = StringMap.add nm arg map in
        let argMap = List.fold_left2 argMapBuilder StringMap.empty fm args in
        let _ = print_endline("argMap: ") in
        let _ = StringMap.iter (fun k v -> print_endline k) argMap in
        let _ = print_endline("argMapDone") in
        let maps = {n=nm; argMap=argMap; cMap=maps.cMap; net=maps.net} in
        let maps = 
            if StringMap.mem maps.n maps.cMap 
            then {n=maps.n;
                  argMap=maps.argMap; 
                  cMap=StringMap.add maps.n 
                      ((StringMap.find maps.n maps.cMap)+1) maps.cMap;
                  net = maps.net }
            else {n=maps.n;
                  argMap=maps.argMap; 
                  cMap= StringMap.add maps.n 0 maps.cMap;
                  net=maps.net} in
        let foldFn maps exp = 
                let collapsedEx = collapseFn maps exp in
                let maps = snd collapsedEx in
                {n=maps.n; argMap=maps.argMap; cMap=maps.cMap; net=(fst collapsedEx)::maps.net} in
        let maps = List.fold_left foldFn maps exps in

        let getOut(*by Jordan Peele*)= 
                let findFn nm (sz,bindName) = nm=bindName in
                collapseFn maps (BoolId(snd (List.find (findFn a) out))) in
        let maps = {n=oldMap.n; argMap=oldMap.argMap; cMap=maps.cMap; net=maps.net} in

        let lst = List.hd (List.rev maps.net) in
        let net = lst::List.rev (List.tl (List.rev maps.net)) in 

        let maps = {n=maps.n; argMap=maps.argMap; cMap=maps.cMap; net=maps.net} in
        (fst getOut, maps)
    | Index (exp, rng) ->
            let exp2 = collapseFn maps exp in
            (Index(fst exp2, rng), snd exp2)
    | Print (str, exp) -> 
            let p = collapseFn maps exp in
            (Print(str, fst p), snd p)
    | Call(str, exp) -> 
            print_endline ("Something is wrong! Call called in elaborate");
            (Noexpr, maps)
    | For(str,r,exprLst) -> 
                    let oldMap = maps in
                    let forFn maps expr = 
                            let cex = collapseFn maps expr in
                            let maps' = snd cex in
                            let cexp = fst cex in
                            {n=maps.n; argMap=maps'.argMap; cMap=maps'.cMap; net=cexp::maps'.net} 
                    in
                    let maps = {n=maps.n; argMap=maps.argMap; cMap=maps.cMap; net=[]} in
                    let exprLst = List.fold_left forFn maps exprLst in
                    let maps = {n=maps.n; argMap=maps.argMap; cMap=exprLst.cMap; net=oldMap.net} in
                    (For(str,r,exprLst.net), maps) 
    | ModExpr(Module_decl(out,nm,fm,exps), args, par) -> 
        let oldMap = maps in
        let parMap =
                 let getNm x = match x with
                    (Some(ModExpr(Module_decl(_,nm,_,_),_,_))) -> nm
                   | None -> ""
                   | Some x -> print_endline("missed caseZ: "^Printer.getBinExpr x); "" in
                 let tag =  if (nm="main") then "_" else getNm par in 
                 {n=tag; argMap=oldMap.argMap; cMap=oldMap.cMap; net=maps.net} in
        let argMapBuilder outmap (sz,nm) arg = StringMap.add nm (fst(collapseFn parMap arg)) outmap in
        let argMap = List.fold_left2 argMapBuilder StringMap.empty fm args in
        let _ = print_endline("argMap: ") in
        let _ = StringMap.iter (fun k v -> print_endline (k^": "^ Printer.getBinExpr v)) argMap in
        let _ = print_endline("argMapDone") in
        let maps = {n=nm; argMap=argMap; cMap=maps.cMap; net=maps.net} in
        let maps = 
            if StringMap.mem maps.n maps.cMap 
            then {n=maps.n;
                  argMap=maps.argMap; 
                  cMap=StringMap.add maps.n 
                      ((StringMap.find maps.n maps.cMap)+1) maps.cMap;
                  net = maps.net }
            else {n=maps.n;
                  argMap=maps.argMap; 
                  cMap= StringMap.add maps.n 0 maps.cMap;
                  net=maps.net} in
        let foldFn maps exp = 
                let collapsedEx = collapseFn maps exp in
                let maps = snd collapsedEx in
                {n=maps.n; argMap=maps.argMap; cMap=maps.cMap; net=(fst collapsedEx)::maps.net} in
        let maps = List.fold_left foldFn maps exps in

        let getOut(*by Jordan Peele*)= 
                if (List.length out >0)
                then collapseFn maps (BoolId(snd (List.hd out)))
                else (Noexpr, maps) in
        let maps = {n=oldMap.n; argMap=oldMap.argMap; cMap=maps.cMap; net=maps.net} in
        (fst getOut, maps)
    (*
    | Noexpr -> Noexpr
    *)
    | x -> print_endline ("we missed a case in elaborate: "^ Printer.getBinExpr x); (Noexpr, maps)


let collapse ast = 
        let strtMap = {n=""; argMap=StringMap.empty; cMap=StringMap.empty; net=[]} in
        List.rev (snd (collapseFn strtMap ast)).net;;


(*-------------------collapse2-------------------*)
let globs = [];;
(*TODO generate these global vars in register step*)

let getStrOrLit = function
      Buslit(x) -> x
    | BoolId(x) -> x
    | x -> print_endline("mc-getStrOrlit: "^Printer.getBinExpr x); ""

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
        | x -> let _ = print_endline("missed case: "^ Printer.getBinExpr x)  in
                    (getStrOrLit(l), "", "", "")
    )
  | Print(nm, r) -> (nm, "Print", getStrOrLit(r), getStrOrLit(r))
  | a -> print_endline ("something else!!"); 
         print_endline (Printer.getBinExpr a);
         ("","","","")

let collapse2 prenet = 
    (List.map collapseFn2 prenet, globs)
