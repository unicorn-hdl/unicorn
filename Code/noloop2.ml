open Ast

exception UndeclaredVar of string 


type data = {i:string; c:int; n:int; o:binExpr list}

let add (Lit(a)) (Lit(b)) = Lit(a+b)
let sub (Lit(a)) (Lit(b)) = Lit(a-b)

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
      | Index(ex, Range(a,b)) -> Index(replace str pos ex, Range(intRep str pos a, intRep str pos b))
      | Print(nm,ex) -> Print(nm, replace str pos ex)
      | Call(_,_) -> print_endline("Error: Call got called in noloop."); Noexpr
      | For(_,_,_) -> print_endline("Error: For got called in noloop-replace."); Noexpr
      | ModExpr(_,_,_) -> print_endline("Error: ModExpr got called in noloop."); Noexpr
      | Noexpr -> Noexpr

let rec loop str curr until outlist exprlist = 
        if (curr <= until)
        then
            let d = {i=str; c=curr; n=until; o=outlist} in
            let d = List.fold_left evalLine d exprlist in
            loop str (curr+1) until d.o exprlist
        else
            {i=str; c=curr; n=until; o=outlist} 

and evalLine d expr= 
      let i = d.i in let c = d.c in
      match expr with
        Buslit(x) -> {i=d.i; c=d.c; n=d.c; o=expr::d.o}
      | BoolId(x) -> {i=d.i; c=d.c; n=d.c; o=expr::d.o}
      | BoolBinop(l,op,r) -> 
                      let newExpr = BoolBinop(replace i c l, op, replace i c r) in 
                      {i=d.i; c=d.c; n=d.c; o=newExpr::d.o}
      | Unop(op,x) -> 
                      let newExpr = Unop(op, replace i c x) in
                      {i=d.i; c=d.c; n=d.c; o=expr::d.o}
      | Assign(isR, l, r, init) -> 
                let newExpr = Assign(isR, replace i c l, replace i c r, init) in
                {i=d.i; c=d.c; n=d.c; o=newExpr::d.o}
      | Index(ex, Range(a,b)) ->
                let newExpr = Index(replace i c ex, Range(intRep i c a, intRep i c b)) in
                {i=d.i; c=d.c; n=d.c; o=newExpr::d.o}
      | Print(nm, ex) ->
                let newExpr = Print(nm, replace i c ex) in
                {i=d.i; c=d.c; n=d.c; o=newExpr::d.o}
      | Call(_,_) -> print_endline("Error: Call got called in noloop."); d
      | For(str,Range(Lit(a),Lit(b)),expList) -> 
                loop str a b d.o expList
      | ModExpr(_,_,_) -> print_endline("Error: ModExpr got called in noloop."); d
      | Noexpr -> d

let dissolveLoop outlist = function
        For(index, Range(Lit(a),Lit(b)), expList) ->
               (loop index a b outlist expList).o
      | x -> outlist

let unloop netlist = List.rev (List.fold_left dissolveLoop [] netlist)
