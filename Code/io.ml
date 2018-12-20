(* Simplifies strings so that programmers
 * don't have to write the long full names
 * of args in/out of the main*)

open Ast

module StringMap = Map.Make(String)
module P = Printer
module S = String

exception InvalidCall of string

(* "main_0_a[2]" -> "a_2" *)
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
        str'^"_"^ix

(* "main_0_a[2]" -> "a" *)
let simple2 str = 
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
        str'

let rec indxLoop a b globs nm =
        if (a < b)
        then  
           let newG = (nm^"_"^(P.sOfI a),"","0") in
           indxLoop (a+1) b (newG::globs) nm
        else
           globs

let iostuff (nlist,globs) fms outs = 
        let rep str = 
                let str = 
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
                if (S.length str > 7)
                then if (S.sub str 0 8 = ".main_0_")
                then simple str 
                else str 
                else str in
        let editFn (a,b,c,d) = 
                let p (_,nm) = (nm = simple2 a) in
                let (a,b,c,d) =
                    if S.contains a '~'
                    then 
                            if (List.exists p fms)
                    then ("","","","")
                    else (a,b,c,d) 
                    else (a,b,c,d) in
                (rep a, rep b, rep c, rep d) in
        let globs = 
                let addFm globs (a,b,c,d) =
                    if S.contains a '~'
                    then (simple a,"","0")::globs
                    else globs in
                List.fold_left addFm globs nlist in
        let globs =
                let addOut globs (Lit(x),nm) = indxLoop 0 x globs nm in
                List.fold_left addOut globs outs in
                
        let globs = List.map (fun (a,b,c) -> (rep a, b, c)) globs in
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
