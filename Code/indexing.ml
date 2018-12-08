open Ast
module StringMap = Map.Make(String)

let id boolin digit = match boolin with
       BoolId(nm) -> BoolId(nm^"["^(string_of_int digit)^"]")
     | Index(BoolId(nm),r) -> BoolId(nm^"["^(string_of_int digit)^"]")
     | x -> print_endline("Missed case in id(indx): "^ Printer.getBinExpr x); x

let id2 boolin digit = 
        let x = id boolin digit in
        match x with BoolId(x) -> x

let rec loop from1 until from2 outlist expr = match expr with
        Assign(isR,la,ra,init) -> (match ra with
          | Buslit(x) ->
                if (from1 <= until)
                then 
                  let digit = String.make 1 (x.[String.length x - from2-2]) in
                  let assig = (Assign(isR, id la from1, Buslit(digit), init)) in
                  loop (from1+1) until (from2+1) (assig::outlist) expr
                else
                  outlist
          | BoolId(x) ->
                if (from1 <= until)
                then
                  let assig = Assign(isR,id la from1, id ra from2, init) in
                  loop (from1+1) until (from2+1) (assig::outlist) expr
                else
                  outlist
          | BoolBinop(l,op,r) -> 
                if (from1 <= until)
                then 
                  let binop = BoolBinop(id l from2, op, id r from2) in
                  let assig = Assign(isR,id la from1, binop, init) in
                  loop (from1+1) until (from2+1) (assig::outlist) expr
                else
                  outlist
          | Unop(op,ex) -> 
                if (from1 <= until)
                then 
                  let unop = (Unop(op, id ex from2)) in
                  let assig = (Assign(isR, id la from1, unop, init)) in
                  loop (from1+1) until (from2+1) (assig::outlist) expr
                else
                  outlist
          | Index(Buslit(x),Range(Lit(a),Lit(b))) ->
                if (from1 <= until)
                then 
                  let digit = String.make 1 (x.[String.length x - from2-2]) in
                  let assig = (Assign(isR, id la from1, Buslit(digit), init)) in
                  loop (from1+1) until (from2+1) (assig::outlist) expr 
                else
                  outlist
          | Index(ex,Range(Lit(a),Lit(b))) ->
                if (from1 <= until)
                then
                  let indx = id ex from2 in
                  let assig = (Assign(isR, id la from1, indx, init)) in
                  loop (from1+1) until (from2+1) (assig::outlist) expr
                else
                  outlist
          | a -> print_endline("missing case: "^ Printer.getBinExpr a); outlist
        ) 
      | Print(nm, ex) -> 
          let nm = BoolId(nm) in
          (match ex with
          | Buslit(x) ->
                if (from1 <= until)
                then 
                  let digit = String.make 1 (x.[String.length x - from2-2]) in
                  let prt = (Print(id2 nm from1, Buslit(digit))) in
                  loop (from1+1) until (from2+1) (prt::outlist) expr 
                else
                  outlist
          | BoolId(x) ->
                if (from1 <= until)
                then
                  let prt = Print(id2 nm from1, id ex from2) in
                  loop (from1+1) until (from2+1) (prt::outlist) expr 
                else
                  outlist
          | BoolBinop(l,op,r) -> 
                if (from1 <= until)
                then 
                  let binop = BoolBinop(id l from2, op, id r from2) in
                  let prt = Print(id2 nm from1, binop) in
                  loop (from1+1) until (from2+1) (prt::outlist) expr
                else
                  outlist
          | Unop(op,ex) -> 
                if (from1 <= until)
                then 
                  let unop = Unop(op, id ex from2) in
                  let prt = Print(id2 nm from1, unop) in
                  loop (from1+1) until (from2+1) (prt::outlist) expr
                else
                  outlist
          | Index(Buslit(x),Range(Lit(a),Lit(b))) ->
                if (from1 <= until)
                then 
                  let digit = String.make 1 (x.[String.length x - from2-2]) in
                  let prt = Print(id2 nm from1, Buslit(digit)) in
                  loop (from1+1) until (from2+1) (prt::outlist) expr
                else
                  outlist
          | Index(ex,Range(Lit(a),Lit(b))) ->
                if (from1 <= until)
                then
                  let indx = id ex from2 in
                  let prt = Print(id2 nm from1, indx) in
                  loop (from1+1) until (from2+1) (prt::outlist) expr
                else
                  outlist
          | a -> print_endline("missing case: "^ Printer.getBinExpr a); outlist
        ) 

  (*
let indicize (outlist,slist) f = (f::outlist, slist)
*)

let rec semant (valz,map) = function
  | Buslit(x) -> (String.length x-1, map)
  | BoolId(x) -> print_endline("looking for: "^x); (StringMap.find x map, map)
  | BoolBinop(l,op,r) -> semant (valz,map) l
  | Unop(op,ex) -> semant (valz,map) ex
  | Assign(_,lval,rval,_) -> (match lval with
         BoolId(x) -> 
                let s = semant (valz,map) rval in
                (fst s, StringMap.add x (fst s) (snd s))
       | Index(BoolId(x),Range(_,Lit(b))) -> 
                      if StringMap.mem x map
                      then 
                        let szx = semant (valz,map) rval in
                        let mx = max (fst szx) b+1 in
                        (mx, StringMap.add x mx map)
                      else ((b+1), StringMap.add x (b+1) map)
  )
  | Print(_,_) -> (valz,map)
  | Index(_,Range(Lit(a),Lit(b))) -> (b-a+1,map)
  | Call(_,_)  -> print_endline ("Something is wrong. Call should not be called in indexing");(valz,map)
  | For(_,_,_) -> print_endline ("Something is wrong. For should not be called in indexing");(valz,map)
  | ModExpr(_,_,_) -> print_endline ("Something is wrong. ModExpr should not be called in indexing");(valz,map)
  | Noexpr -> print_endline ("Something is wrong. Noexpr should not be called in indexing");(valz,map)
  | x -> print_endline ("Missed case (indexing): "^ Printer.getBinExpr x); (valz,map)
                   
let indicize (outlist,slist) line = 
            let from2 x = (match x with  
            Buslit(x) -> 0
          | BoolId(x) -> 0
          | BoolBinop(l,op,r) -> 0
          | Unop(op,ex) -> 0
          | Index(Buslit(x),Range(Lit(a),Lit(b))) -> a
          | Index(ex,Range(Lit(a),Lit(b))) -> a
          | x -> print_endline ("MIssed case-indexingr: "^ Printer.getBinExpr x); 0
            ) in
    match line with
    Buslit(x) -> (outlist,slist)
  | Assign(isR,l,r,init) -> 
            let from2 = from2 r in
            (match l with
            Index(BoolId(x),Range(Lit(a),Lit(b))) ->
                let _ = print_endline("inAssign: "^x^" "^string_of_int a^" "^string_of_int b^" "^string_of_int from2) in
                (loop a b from2 outlist (Assign(isR,l,r,init)), slist)
          | BoolId(x) -> 
                let sz = StringMap.find x slist in
                let _ = print_endline("inAssign: "^x^" "^string_of_int (sz-1)^" "^string_of_int from2) in
                (loop 0 (sz-1) from2 outlist (Assign(isR,l,r,init)), slist)
          | x -> print_endline ("MIssed case-indexingl: "^ Printer.getBinExpr x); ([],slist)
            )
  | Print(nm,ex) -> 
            let from2 = from2 ex in
            let sz = semant (0,slist) ex in
            (loop 0 (fst sz-1) from2 outlist (Print(nm, ex)), slist)
  | x -> print_endline("Missed case in indexing: "^ Printer.getBinExpr x); (outlist,slist)

let printf k v = print_endline(k^ ": "^ (string_of_int v))

let index netlist = 
        let slist = snd(List.fold_left semant (0, StringMap.empty) netlist) in
        let _ = StringMap.iter printf (slist) in
        List.rev (fst(List.fold_left indicize ([],slist) netlist))
