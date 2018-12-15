open Ast

module StringMap = Map.Make(String)
module P = Printer
module S = String

exception InvalidCall of string



let iostuff (nlist,globs) fms outs = 
        let simple str = 
            let str0 = str in
            let a = (S.index str '_')+1 in
            let b = (S.length str) - a in
            let str = S.sub str a b in
            let a = (S.index str '_')+1 in
            let b = (S.length str) - a in
            let str = S.sub str a b in
            let str' = S.sub str 0 (S.index str '[') in
            let endX = S.length str0 - S.index str0 ']' in
            let ix = S.sub str0 (S.index str0 '['+1) endX in
            str'^"_"^ix in
        let rep str = 
                if S.contains str '~'
                then 
                  let str0 = str in
                  let a = (S.index str '_')+1 in
                  let b = (S.length str) - a in
                  let str = S.sub str a b in
                  let a = (S.index str '_')+1 in
                  let b = (S.length str) - a in
                  let str = S.sub str a b in
                  let str' = S.sub str 0 (S.index str '[') in
                  let endX = S.length str0 - S.index str0 ']' in
                  let ix = S.sub str0 (S.index str0 '['+1) endX in
                       let p (_,nm) = (nm = str') in
                  if (List.exists p fms) || (List.exists p outs)
                  then str'^"_"^ix 
                  else str0
                else str in
        let editFn (a,b,c,d) = 
                let (a,b,c,d) =
                    if S.contains a '~'
                    then ("","","","")
                    else (a,b,c,d) in
                (rep a, rep b, rep c, rep d) in
        let globs = 
                let globFn globs (a,b,c,d) =
                        if S.contains a '~'
                        then (simple a,"","0")::globs
                        else globs in
                List.fold_left globFn globs nlist in
        (List.map editFn nlist, globs)

let getFms (ModExpr(MD(_,_,_,lns),_)) = match (List.rev lns) with
    ModExpr(MD(_,_,fms,_),_)::tl -> fms

let getOuts (ModExpr(MD(_,_,_,lns),_)) = match (List.rev lns) with 
    ModExpr(MD(outs,_,_,_),_)::tl -> outs

let rec loop a b str = 
        if (a<b) 
        then 
           loop (a+1) b ("0"^str)
        else
           str

let makeVars (MD(_,_,fms,_)) = 
        let mapFn (intX,nm) = match intX with
            Lit(x) -> 
                    let lit = loop 0 x "b" in
                    Assign(false, BoolId(nm), Buslit(lit),"")
          | _ -> raise (InvalidCall ("You may only use literal sizes for arguments in the main")) in
        List.map mapFn fms

let getMainArgs (MD(_,_,fms,_)) = 
        let mapFn (_, nm) = BoolId(nm) in 
        List.map mapFn fms
