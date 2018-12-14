open Ast

type maps = {r: binExpr; o: binExpr list; n: int}
(* r for return,
 * o for outlist,
 * n for currentNum*)

let isS = function
        | Buslit(_) -> true
        | BoolId(_) -> true
        | _ -> false

let toId x = BoolId("#"^ string_of_int x)

let rec simpExp m = function
        | Buslit(x) -> {r=Buslit(x); o=m.o; n=m.n}
        | BoolId(x) -> {r=BoolId(x); o=m.o; n=m.n}
        | BoolBinop(l, op, r) ->
              let l' = 
                if isS l
                then {r=l; o=m.o; n=m.n}
                else 
                  let sl = simpExp m l in
                  let id = toId sl.n in
                  let o' = (Assign(false, id, sl.r, "0"):: sl.o) in
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
                  let o' = Assign(false, toId sr.n, sr.r, "0"):: sr.o in
                  let n' = sr.n+1 in
                  {r=id; o=o'; n=n'}

              in
              let newExpr = BoolBinop(l'.r, op, r'.r) in
              {r=newExpr; o=r'.o; n=r'.n}
        | Unop(op,e) -> 
              if isS e
              then {r=Unop(op,e); o=m.o; n=m.n}
              else 
                let se = simpExp m e in
                let id = toId se.n in
                let o' = (Assign(false, id, se.r, "0"):: se.o) in
                {r=Unop(op,id); o=o'; n=se.n+1}
        | Assign(isR, l, r, init) -> 
              let sr = simpExp m r in
              let init =
                   if isR
                   then String.sub init 0 ((String.length init)-1)
                   else init in
              let newExpr = Assign(isR, l, sr.r, init) in
              {r=l; o=newExpr::sr.o; n=sr.n}
        | Index(ex, r) -> 
              if isS ex
              then {r=Index(ex,r); o=m.o; n=m.n}
              else 
                let sex = simpExp m ex in
                let id = toId sex.n in
                let o' = (Assign(false, id, sex.r, "0")::sex.o) in
                {r=Index(id,r); o=o'; n=sex.n+1}
        | Print(nm, ex) ->
              if isS ex
              then 
                let newExp = Print(nm,ex) in
                {r=newExp; o=newExp::m.o; n=m.n}
              else 
                let sex = simpExp m ex in
                let id = toId sex.n in
                let o' = (Assign(false, id, sex.r, "0")::sex.o) in
                let newExp = Print(nm,id) in
                {r=newExp; o=newExp::o'; n=sex.n+1}
        | Call(str, exp) -> print_endline ("Something is wrong! Call called in simplelines");m
        | For(_,_,_)     -> print_endline ("Something is wrong! For called in simplelines");m
        | ModExpr(_,_,_) -> print_endline ("Something is wrong! ModExpr called in simplelines");m
        | Noexpr -> {r=Noexpr; o=m.o; n=m.n}
        | x -> print_endline ("we missed a case in simplelines: "^ Printer.getBinExpr x);m

let nullmap = {r=Noexpr; o=[]; n=0};;
let simplify nlist = List.rev (List.fold_left simpExp nullmap nlist).o
