open Ast

type maps = {r: binExpr; o: binExpr list; n: int}
(* r for return,
 * o for outlist,
 * n for currentNum*)

let isS = function
        | Buslit(_) -> true
        | BoolId(_) -> true
        | _ -> false

let isSA = function
        | Buslit(_) -> true
        | BoolId(_) -> true
        | BoolBinop(a,_,b) -> (isS a) & (isS b)
        | Unop(_,a) -> isS a
        | _ -> false

let toId x = BoolId("#"^ string_of_int x)

let rec simpExp m = function
        | Buslit(x) -> {r=Buslit(x); o=m.o; n=m.n}
        | Assign(isR, l, r, init) -> 
              let sr = simpExp m r in
                        let newExpr = Assign(isR, l, sr.r, init) in
                        {r=l; o=newExpr::sr.o; n=m.n}
              (*
                      if isSA sr.r
                      then 
                        let newExpr = Assign(isR, l, sr.r, init) in
                        {r=l; o=newExpr::sr.o; n=m.n}
                      else 
                        let id = toId (sr.n) in
                        let newExp = Assign(isR, l, id, init) in
                        {r= id; o= newExp::sr.o; n=m.n}
                        *)

        | BoolBinop(l, op, r) ->
              let l' = 
                if isS l
                then {r=l; o=m.o; n=m.n}
                else 
                  let sl = simpExp m l in
                  let id = toId sl.n in
                  let o' = (Assign(false, id, sl.r, false):: sl.o) in
                  let n' = (sl.n)+1 in
                  {r=id; o=o'; n=n'}

                in
              let m' = {r=m.r; o=l'.o; n=l'.n} in
              let r' = 
                if isS r
                then {r=r; o=m'.o; n=m'.n}
                else 
                  let sr = simpExp m' r in
                  let id = toId sr.n in
                  let o' = Assign(false, toId sr.n, sr.r, false):: sr.o in
                  let n' = sr.n+1 in
                  {r=id; o=o'; n=n'}

              in
              let newExpr = BoolBinop(l'.r, op, r'.r) in
              {r=newExpr; o=r'.o; n=r'.n}

        | a -> {r=m.r; o= a::m.o; n= m.n}

let nullmap = {r=Noexpr; o=[]; n=0};;
let simplify nlist = List.rev (List.fold_left simpExp nullmap nlist).o
