(* Despite having once been very aesthetically pleasant,
 * Noloops largely met its demise at the hands Semant, which
 * more or less ate it up (turns out it's easier to collapse 
 * for loops while hardening/semantic checking). It now serves
 * to just host some helper methods for semant, that engorged
 * tyrant*)

open Ast
open Printer

exception UndeclaredVar of string 

let add (Lit(a)) (Lit(b)) = Lit(a+b)
let sub (Lit(a)) (Lit(b)) = Lit(a-b)

let p x = print_endline(x) 

let rec intRep str pos intExp = match intExp with
        Lit(x) -> Lit(x)
      | IntId(x) -> 
            if (x = str)
            then Lit(pos)
            else raise(UndeclaredVar ("Variable \"" ^ x ^ "\" is not defined!"))
      | IntBinop(l,op,r) ->
            let l = intRep str pos l in
            let r = intRep str pos r in
            if (op = Add)
            then add l r
            else sub l r

let rec replace str pos exp = match exp with
        Buslit(x) -> exp
      | BoolId(x) -> exp
      | BoolBinop(l,op,r) -> BoolBinop(replace str pos l, op, replace str pos r)
      | Unop(op,x) -> Unop(op, replace str pos x)
      | Assign(isR, l, r, init) -> (match l with
            Index(BoolId(x), Range(a,b)) -> 
                    let l = Index(BoolId(x), Range(intRep str pos a, intRep str pos b)) in
                    Assign(isR, l, replace str pos r, init)
          | BoolId(x) -> Assign(isR, l, replace str pos r, init)
       )
      | Index(ModExpr(dec,args),rng) ->
            let args= List.map (replace str pos) args in
            Index(ModExpr(dec,args),rng) 
      | Index(ex, Range(a,b)) -> Index(replace str pos ex, Range(intRep str pos a, intRep str pos b))
      | Print(nm,ex) -> Print(nm, replace str pos ex)
      | Call(_,_) -> print_endline("Error: Call got called in noloop."); Noexpr
      | For(_,_,_) -> print_endline("Error: For got called in noloop-replace."); Noexpr
      | ModExpr(dec,args) -> 
            let args= List.map (replace str pos) args in
            ModExpr(dec,args)
      | Noexpr -> Noexpr
