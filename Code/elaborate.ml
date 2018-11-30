open Ast
module StringMap = Map.Make(String)

(*Maps should be (thisMod'sName, varsInThisMod, howManyTimesHasModNameBeenCollapsed)*)
type mapList = {name: string; argMap: binExpr StringMap.t; countMap: int StringMap.t; net: binExpr list}

let listInvert (lst, b) = List.map (fun a->(a,b)) lst


let rec collapseFn maps exp = match exp with  
    | Buslit(x) -> (Buslit(x), maps)
    | BoolId(x) -> let newX = 
                    if (StringMap.mem x maps.argMap)
                        then StringMap.find x maps.argMap 
                        else BoolId(x) in
                    let newerX = match newX with
                         BoolId(x) ->
                            BoolId (maps.name ^ "_" ^ string_of_int (StringMap.find maps.name maps.countMap) ^ "_" ^ x)
                       | a -> a in
                    (newerX, maps)
    | BoolBinop(lval, op, rval) -> 
            let l2 = collapseFn maps lval in
            let r2 = collapseFn (snd l2) rval in
            (BoolBinop(fst l2, op, fst r2), snd r2)
    | Unop(op, exp) -> (Unop(op, exp), maps)
(*Remember not to collapse on lval of assign. If lval collides with an arg, remove arg from list*)
    | Assign(isReg, lval, rval, init) -> 
            let r2 = collapseFn maps  rval in
            let l2 = collapseFn (snd r2) lval in
            (Assign(isReg, fst l2, fst r2, init), snd l2)
    (*
    | Index(exp, rng) -> Index(exp, rng)
    | Print(str, exp) -> Print(str, exp)
    | Call(str, exp) -> print_endline ("Something is wrong! Call called in elaborate");
                        Call(str, exp) 
    | For(str, rng, exp) -> For(str, rng, exp)
*)
    | ModExpr(Module_decl(out,nm,fm,exps), args, par) -> 
        let fold2Fn map (sz,nm) arg = StringMap.add nm arg map in
        let oldMap = maps in
        let argMap = List.fold_left2 fold2Fn StringMap.empty fm args in
        let maps = {name=nm; argMap=argMap; countMap=maps.countMap; net=maps.net} in
        let maps = 
            if StringMap.mem maps.name maps.countMap 
            then {name=maps.name;
                  argMap=maps.argMap; 
                  countMap=StringMap.add maps.name 
                      ((StringMap.find maps.name maps.countMap)+1) maps.countMap;
                  net = maps.net }
            else {name=maps.name;
                  argMap=maps.argMap; 
                  countMap= StringMap.add maps.name 0 maps.countMap;
                  net=maps.net} in
        let foldFn maps exp = 
                let collapsedEx = collapseFn maps exp in
                let maps = snd collapsedEx in
                {name=maps.name; argMap=maps.argMap; countMap=maps.countMap; net= fst collapsedEx :: maps.net} in
        let maps = List.fold_left foldFn maps exps in

        let getOut(*by Jordan Peele*)= 
                if (List.length out >0)
                then collapseFn maps (BoolId(snd (List.hd out)))
                else (Noexpr, maps) in
        let maps = {name=oldMap.name; argMap=oldMap.argMap; countMap=maps.countMap; net=maps.net} in
        (fst getOut, maps)
    (*
    | Noexpr -> Noexpr
    *)
    | _ -> print_endline ("we missed a case in elaborate"); (Noexpr, maps)
let collapse ast = 
        let strtMap = {name=""; argMap=StringMap.empty; countMap=StringMap.empty; net=[]} in
        (snd (collapseFn strtMap (ModExpr(ast,[],emptyMod)))).net
