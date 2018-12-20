(* Takes in a netlist of shallow binExprs and replaces all
 * buses with n boolean variables. Accounts for indexing.
 *)

open Ast
open Printer
module StringMap = Map.Make(String)

exception Error of string

(*---------helper functions--------*)
let getI pos str =
        if (String.length str > pos)
        then String.make 1 str.[pos]
        else "0"

let id boolin digit = match boolin with
       BoolId(nm) -> BoolId(nm^"["^(string_of_int digit)^"]")
     | Index(BoolId(nm),r) -> BoolId(nm^"["^(string_of_int digit)^"]")
     | Buslit(x) -> Buslit(getI digit x) 
     | x -> p("Missed case in id(indx): "^ Printer.getBinExpr "" x); x

let id2 boolin digit = 
        let x = id boolin digit in
        match x with BoolId(x) -> x
(*---------------------------------*)


let rec loop from1 until from2 outlist expr = match expr with
        Assign(isR,la,ra,init) -> 
          let init = getI from1 init in
          (match ra with
          | Buslit(x) ->
                if (from1 <= until)
                then 
                  let ix = String.length x - from2-2 in
                  let _ = 
                    if ix < 0
                    then raise (Error ("Something is wrong with assignment to "^getBinExpr "" la^". Check that it's not double-assigned"))
                    else () in
                  let digit = String.make 1 (x.[ix]) in
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
          | a -> p("missing case: "^ Printer.getBinExpr "" a); outlist
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
          | a -> p("missing case: "^ Printer.getBinExpr "" a); outlist
        ) 

(* Simplified version of semantic checking— returns size of argument*)
let rec semant n (valz,map) = function
  | Buslit(x) -> (String.length x-1, map)
  | BoolId(x) -> 
        if StringMap.mem x map
        then (StringMap.find x map, map)
        else 
          badSearch map n x
  | BoolBinop(l,op,r) -> semant n (valz,map) l
  | Unop(op,ex) -> semant n (valz,map) ex
  | Assign(isR,lval,rval,init) -> (match lval with
         BoolId(x) -> 
                let s = semant n (valz,map) rval in
                (fst s, StringMap.add x (fst s) (snd s))
       | Index(BoolId(x),Range(_,Lit(b))) -> 
                      if StringMap.mem x map
                      then 
                        let szx = StringMap.find x map in
                        let mx = max szx (b+1) in
                        let map = StringMap.add x mx map in
                        (mx, map) 
                      else 
                        let map = StringMap.add x (b+1) map in
                        ((b+1), map)
        )
  | Print(_,_) -> (valz,map)
  | Index(_,Range(Lit(a),Lit(b))) -> (b-a+1,map)
  | Call(_,_)  -> p ("Something is wrong. Call should not be called in indexing");(valz,map)
  | For(_,_,_) -> p ("Something is wrong. For should not be called in indexing");(valz,map)
  | ModExpr(_,_) -> p ("Something is wrong. ModExpr should not be called in indexing");(valz,map)
  | Noexpr -> p ("Something is wrong. Noexpr should not be called in indexing");(valz,map)
  | x -> p ("Missed case (indexing): "^ Printer.getBinExpr "" x); (valz,map)
                   
(*In case things got moved around during collapse, run a search
 * through the entire netlist for semantic checking, if n hasn't been found yet*)
and badSearch map n str =
   let badfold (valz,map) expr = match expr with
     Assign(_,lval,rval,_) -> 
        (match lval with
        BoolId(x) ->
                if (x=str)
                then 
                  let s = semant n (valz,map) rval in
                  (fst s, StringMap.add x (fst s) (snd s))
                else
                  (valz,map)
      | Index(BoolId(x),Range(_,Lit(b))) ->
                if (x=str)
                then 
                  if StringMap.mem x map
                  then 
                    let szx = semant n (0,map) rval in
                    let mx = max (fst szx) (b+1) in
                    (mx, StringMap.add x mx map)
                  else ((b+1), StringMap.add x (b+1) map)
                else
                  (valz,map)
        )
  | x -> (valz,map) in
  List.fold_left badfold (0,map) n

(* replace line with its indexed version*)
let indicize (outlist,slist) line = 
            let from2 x = (match x with  
            Buslit(x) -> 0
          | BoolId(x) -> 0
          | BoolBinop(l,op,r) -> 0
          | Unop(op,ex) -> 0
          | Index(Buslit(x),Range(Lit(a),Lit(b))) -> a
          | Index(ex,Range(Lit(a),Lit(b))) -> a
          | x -> p ("Missed case-indexingr: "^ Printer.getBinExpr "" x); 0
            ) in
    match line with
    Buslit(x) -> (outlist,slist)
  | Assign(isR,l,r,init) -> 
            let from2 = from2 r in
            (match l with
            Index(BoolId(x),Range(Lit(a),Lit(b))) ->
                (loop a b from2 outlist (Assign(isR,l,r,init)), slist)
          | BoolId(x) -> 
                let sz = StringMap.find x slist in
                (loop 0 (sz-1) from2 outlist (Assign(isR,l,r,init)), slist)
          | x -> p ("Missed case-indexingl: "^ Printer.getBinExpr "" x); ([],slist)
            )
  | Print(nm,ex) -> 
            let from2 = from2 ex in
            let sz = semant [] (0,slist) ex in
            (loop 0 (fst sz-1) from2 outlist (Print(nm, ex)), slist)
  | x -> p("Missed case in indexing: "^ Printer.getBinExpr "" x); (outlist,slist)

(* Get sizes of registers before everything else*)
let presemant outmap = function
            Assign(true,BoolId(x),_,init) -> 
                    StringMap.add x (String.length init) outmap
          | x -> outmap

let index netlist = 
        let slist = List.fold_left presemant StringMap.empty netlist in
        let slist = snd(List.fold_left (semant netlist) (0, slist) netlist) in
        List.rev (fst(List.fold_left indicize ([],slist) netlist))
