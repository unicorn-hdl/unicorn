open Ast
open Printer
open Harden2
open ERR 
module StringMap = Map.Make(String)

exception InvalidAssignment of string
exception UndeclaredVar of string 
exception TypeMismatch of string
exception InvalidRange of string
exception InvalidCall of string

type d = {m: int StringMap.t; x:binExpr list; s:int}
(* m is name map
 * x is return value (i.e. the new expression)
 * s is size of returned expr
 * *)

let getLit exp = match exp with
       Lit(x) -> x
     | x -> p ("missed case: "^ Printer.getIntExpr x); 0

(*some utility fns*)
let printMap map name=
        print_endline ("\nprinting map (" ^ name ^ ")");
        StringMap.iter (fun k v -> print_string(k ^"<"^string_of_int v^">, ")) map;
        print_endline ("\ndone printing map");;

let lookup str map =
       try StringMap.find str map
       with Not_found -> -1

let assignIsValid lval = match lval with
      BoolId(_) -> () (*valid*)
    | Index(BoolId(_), _) -> () (*valid*)
    | x -> raise(InvalidAssignment("\"" ^ Printer.getBinExpr "" x ^ "\" may not be assigned to"))
    
let lengthsAreValid fms args name = 
      if List.length fms = List.length args
      then ()
      else icERR name fms args

let rangeIsValid (Lit a) (Lit b) = 
    if (a<=b && a>=0)
    then ()
    else raise(InvalidRange ("The range (" ^ string_of_int a ^ ", " ^ string_of_int b ^ ") is invalid!"))

let rec findRegs outMap expr = 
    let foldFn map ex = findRegs map ex in
    match expr with
    | Buslit(x) -> outMap
    | BoolId(x) -> outMap
    | BoolBinop(l,_,r) ->
            let rmap = findRegs outMap r in
            findRegs rmap l 
    | Unop(_,x) -> findRegs outMap x 
    | Assign(true,BoolId(x),r,init) ->
            let rmap = findRegs outMap r in
            StringMap.add x (String.length init -1) outMap
    | Assign(false,_,r,_) -> findRegs outMap r
    | Index(x,_) ->findRegs outMap x
    | Print(_,x) -> findRegs outMap x
    | Call(nm,_) -> print_endline("Call "^nm^" got called in semant"); outMap
    | For(_,_,exprs) -> List.fold_left foldFn outMap exprs
    | ModExpr(MD(_,_,_,exprs),args) -> 
            let argMap = List.fold_left foldFn outMap args in 
            List.fold_left foldFn argMap exprs
    | Noexpr -> outMap


let rec validArg arg = match arg with
    | BoolId(x) -> ()
    | Buslit(x) -> ()
    | BoolBinop(l,_,r) -> (validArg l; validArg r)
    | Unop(_,x) -> validArg x
    | Assign(_,_,_,_) -> raise (InvalidCall("Assignments may not be arguments"))
    | Index(x,_) -> validArg x
    | Print(_,_) -> raise (InvalidCall("Print statements may not be arguments"))
    | Call (_,_) -> p "Something is wrong! Call called in semant"
    | For(_,_,_) -> raise (InvalidCall("For loops may not be arguments"))
    | ModExpr(_,_) -> raise (InvalidCall("Module calls may not be arguments"))
    | Noexpr -> raise (InvalidCall("Noexpr may not be an argument"))

let rec check d x = 
        match x with  
      Buslit(valz) -> {m=d.m; x=[x]; s=String.length valz -1}
    | BoolId(name) -> 
        let s =
            if StringMap.mem name d.m
            then lookup name d.m
            else uvERR name in
        {m=d.m; x=[x]; s=s}
    | BoolBinop(l, op, r) -> 
        let l = hardenline d.m l in
        let r = hardenline d.m r in
        let ld = check  d l in
        let rd = check ld r in
        let x = BoolBinop(List.hd ld.x,op,List.hd rd.x) in
        let s = 
            if (ld.s= rd.s)
            then ld.s 
            else tmERR (B  op) l r in
        {m=rd.m; x=[x]; s=s}
    | Unop(op, x) -> 
        let x = hardenline d.m x in
        let d = check d x in
        let x = Unop(op,List.hd d.x) in
        {m=d.m; x=[x]; s=d.s}
    | Assign(isR, lval, rval, init) ->
        let lval = hardenline d.m lval in
        let rval = hardenline d.m rval in
        let _ = assignIsValid lval in
        let rd = check d rval in
        let d = 
          (match lval with
          | BoolId(x) -> 
             let m = StringMap.add x rd.s rd.m in
             {m=m; x=[]; s=rd.s}
          | Index(BoolId(x), Range(a,b)) -> 
             let b' = getLit b in
             let a' = getLit a in
             let _ =  
                 if (rd.s = (b'-a'+1))
                 then ()
                 else tm_assERR rval rd.s x a' b' in
             let s = max (lookup x rd.m) (b'+1) in
             let m = StringMap.add x s rd.m in
             {m=m; x=[]; s=s}
          ) in
        let x = Assign(isR,lval,List.hd rd.x,init) in
        {m=d.m; x=[x]; s=d.s}
    | Index(ModExpr(MD(outs,nm,fm,lns),args), Range(a,b)) ->
        let oldMap = d in
        let _ = 
           if (a = b)
           then ()
           else raise(InvalidRange ("You can only access one output from a module at a time!")) in
        let selected = 
          match a with
          | Lit(x) -> 
                if (List.length outs > x)
                then snd(List.nth outs x)
                else irERR x nm outs
          | IntId(x) ->
                if (List.exists (fun (_,a) -> a=x) outs)
                then x
                else raise(InvalidRange("ERROR: You tried to access "^x^" but "^nm^" has no such outputs!")) in
        let x = ModExpr(MD(outs,nm,fm,lns),args) in
        let d = checkMod x d selected in
        let x = Index(List.hd d.x,Range(a,b)) in
        {m=oldMap.m; x=[x]; s=d.s}
    | Index(x, Range(a,b)) -> 
        let a' = getLit (eval d.m a) in
        let b' = getLit (eval d.m b) in
        let _ = rangeIsValid (Lit(a')) (Lit(b')) in
        let x = hardenline d.m x in
        let d = check d x in
        let s = 
            if (d.s > b')
            then b'-a'+1
            else raise(TypeMismatch ("ERROR: You tried accessing the "^sOfI b'^"th element of a "^sOfI d.s^"-bit bus")) in
        let x = Index(x, Range(Lit(a'),Lit(b'))) in
        {m=d.m; x=[x]; s=s}
    | Print(nm, x) -> 
        let x = hardenline d.m x in
        let d = check d x in
        let x = Print(nm, List.hd d.x) in
        let s = 1 in
        {m=d.m; x=[x]; s=s}
    | Call(_) -> print_endline ("Call is showing up in check"); d
    | For(str, Range(a,b), lines) ->
                    (*TODO return the correct x here*)
        let a = eval d.m a in
        let b = eval d.m b in
        let _ = rangeIsValid a b in

        let a = getLit a in
        let b = getLit b in 
        let d = {m=d.m; x=[]; s=d.s} in
        let linesFn x = List.map (Noloop2.replace str x) lines in
        let foldFn d line = 
                let line = hardenline d.m line in
                let d2 = check d line in
                {m=d2.m; x=d.x@d2.x; s=d2.s} in
        let rec loop a b d = 
                if (a <=b) 
                then
                   let d = List.fold_left foldFn d (linesFn a) in
                   loop (a+1) b d 
                else d in
        let d = loop a b d in
        (*
        let _ = p "Printing for loop" in
        let _ = List.iter (fun x-> p (getBinExpr "" x)) d.x in
        let _ = p "\n\n" in
*)
        {m=d.m; x=d.x; s=0}
    | ModExpr(MD(out,_,_,_), _) -> 
        let oldMap = d in
        (*When ModExpr has no index, it just returns its first out*)
        let selected = 
            if (List.length out> 0)
            then snd(List.hd out)
            else "" in
        let d = checkMod x d selected in
        {m=oldMap.m; x=d.x; s=d.s}
    | Noexpr -> d
    | a -> print_endline("missing case in checkvalidities: " ^ getBinExpr "" a ^ "DONE\n") ; d


and checkMod (ModExpr(MD(out,name,fms,exprs), args)) d selected= 
        (*
        let _  = p name in
*)
        let oldD = d in

        let _ = lengthsAreValid fms args name in
        let _ = List.iter validArg args in

        (*Add generics and registers to m. Also harden args*)
        let x = List.fold_left2 (tableFn oldD) (StringMap.empty,[]) fms args in 
        let m = fst x in
        let args = List.rev(snd x) in
        let m = List.fold_left findRegs m exprs in
        let d = {m=m; x=[]; s=d.s} in

        (*Call check on every line in mod*)
        let foldFn d line = 
                let d2 = check d line in
                {m=d2.m; x=d.x@d2.x; s=d2.s} in
        let d = List.fold_left foldFn d exprs in

        let checkOut (intEx,nm) =
            let fmS = getLit(eval d.m intEx) in
            let argS = lookup nm d.m in
            let _ = 
                if (argS = -1) 
                then raise(UndeclaredVar ("The output call "^ nm^ " is never defined"))
                else () in
            if (argS = fmS)
            then (Lit(fmS), nm)
            else tm_outERR (intEx,nm) argS in
        let out = List.map checkOut out in
        let s = lookup selected d.m in

        let x = ModExpr(MD(out,name,fms,d.x),args) in
        {m=d.m; x=[x]; s=s}

and tableFn oldD (m,newArgs) (fmX,nm) arg = 
     let oldArg = arg in
     let arg = (check oldD arg) in
     match fmX with
     | IntId(x) -> 
            let x = "*"^x in
            let m =
               if StringMap.mem x m
               then 
                    let fmS = StringMap.find x m in
                    if (fmS = arg.s)
                    then m
                    else tm_argERR oldArg arg.s nm fmS
               else StringMap.add x arg.s m in
            let m = StringMap.add nm arg.s m in
            (m, arg.x@newArgs)
     | x -> 
            let fmS = getLit(eval oldD.m fmX) in
            let _ =
                if (fmS = arg.s)
                then ()
                else tm_argERR oldArg arg.s nm fmS in
            let m = StringMap.add nm arg.s m in
            (m, arg.x@newArgs)

let semant hast =
    let d = {m=StringMap.empty; x=[]; s=0} in
    List.hd ((check d hast).x)
