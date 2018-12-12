open Ast
open Printer
open Harden2
module StringMap = Map.Make(String)

exception InvalidAssignment of string
exception UndeclaredVar of string 
exception TypeMismatch of string
exception InvalidRange of string
exception InvalidCall of string

type d = {m: int StringMap.t; x:binExpr list; s:int}

let getLit exp = match exp with
       Lit(x) -> x
     | x -> p ("missed case: "^ Printer.getIntExpr x); 0

(*Moving error messages out of the way, for readability*)
let uvERR name = raise (UndeclaredVar ("Variable \"" ^ name ^ "\" is not defined!"))

let tmERR op l r = raise(TypeMismatch ("You tried performing " ^ opToStr op ^ " on " ^ Printer.getBinExpr l ^" and "^Printer.getBinExpr r^" but these are of different sizes"))

let tm_assERR rval rtyp x a' b'= raise(TypeMismatch ("You tried assigning " ^ Printer.getBinExpr rval^ " of size " ^ string_of_int rtyp ^ " to " ^  x ^ " on the range " ^ string_of_int a' ^ "-" ^ string_of_int b' ^ " but these are of different sizes"))

let irERR x nm outs = raise (InvalidRange ("ERROR: You tried to access the "^string_of_int x^"th element of "^nm^" but it only has "^string_of_int (List.length outs)^" outputs!"))

let tm_argERR arg argS fm fmS = raise(TypeMismatch ("You tried assigning argument " ^ Printer.getBinExpr arg^ " of size "^ string_of_int argS ^ " to formal " ^ fm ^ "<" ^ string_of_int fmS ^ "> but these are of different sizes"))

let tm_outERR out sz = raise(TypeMismatch ("The output call "^ snd out ^"<"^ string_of_int (getLit (fst out)) ^ "> does not match assignment of size "^ string_of_int sz)) 

let icERR name fms args= raise(InvalidCall ("Call to " ^ name ^ " with " ^ string_of_int (List.length args)
        ^ " arguments but " ^ name ^ " expects " ^ string_of_int (List.length fms) ^ " arguments."))

(*some utility fns*)
let printMap map name=
        print_endline ("\nprinting map (" ^ name ^ ")");
        StringMap.iter (fun k v -> print_string(k ^"<"^string_of_int v^">, ")) map;
        print_endline ("\ndone printing map");;

let lookup str map =
       try StringMap.find str map
       with Not_found -> -1

let rec evalInt = function
 | Lit(x) -> x
 | IntId(_) -> 1 (*have to have evalInt x here, but really evalInt x depends on a values table (and will be hard to implement)*)
 | IntBinop(a,Add,b) -> evalInt a + evalInt b
 | IntBinop(a,Sub,b) -> evalInt a - evalInt b

let evalBind (a,b) = (string_of_int (evalInt a), b)

let assignIsValid lval = match lval with
      BoolId(_) -> () (*valid*)
    | Index(BoolId(_), _) -> () (*valid*)
    | x -> raise(InvalidAssignment("\"" ^ Printer.getBinExpr x ^ "\" may not be assigned to"))

let rangeIsValid (Lit a) (Lit b) = 
    if (a<=b && a>=0)
    then ()
    else raise(InvalidRange ("The range (" ^ string_of_int a ^ ", " ^ 
                                string_of_int b ^ ") is invalid!"))


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
    | Call(_,_) -> print_endline("Call got called in semant"); outMap
    | For(_,_,exprs) -> List.fold_left foldFn outMap exprs
    | ModExpr(MD(_,_,_,exprs),args,_) -> 
            let argMap = List.fold_left foldFn outMap args in 
            List.fold_left foldFn argMap exprs
    | Noexpr -> outMap


let rec check d x = 
        match x with  
      Buslit(valz) -> {m=d.m; x=[x]; s=String.length valz -1}
    | BoolId(name) -> 
    (*TODO: Need a "badsearch" in case var hasn't been decl'd yet*)
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
        let x = BoolBinop(l,op,r) in
        let s = 
            if (ld.s= rd.s)
            then ld.s 
            else tmERR (B  op) l r in
        {m=d.m; x=[x]; s=s}
    | Unop(op, x) -> 
        let x = hardenline d.m x in
        let d = check d x in
        let x = Unop(op,x) in
        {m=d.m; x=[x]; s=d.s}
    | Assign(isR, lval, rval, init) ->
        let lval = hardenline d.m lval in
        let rval = hardenline d.m rval in
        let _ = assignIsValid lval in
        let rd = check d rval in
        let d = 
          (match lval with
          | BoolId(x) -> 
             let m = StringMap.add x rd.s d.m in
             {m=m; x=[]; s=rd.s}
          | Index(BoolId(x), Range(a,b)) -> 
             let b' = getLit b in
             let a' = getLit a in
             let _ =  
                 if (rd.s = (b'-a'+1))
                 then ()
                 else tm_assERR rval rd.s x a' b' in
             let s = max (lookup x d.m) (b'+1) in
             let m = StringMap.add x s d.m in
             {m=m; x=[]; s=s}
          ) in
        let x = Assign(isR,lval,List.hd rd.x,init) in
        {m=d.m; x=[x]; s=d.s}
    | Index(ModExpr(MD(outs,nm,fm,lns),args,par), Range(a,b)) ->
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
        let x = ModExpr(MD(outs,nm,fm,lns),args,par) in
        let d = checkMod x d selected in
        let x = Index(List.hd d.x,Range(a,b)) in
        {m=d.m; x=[x]; s=d.s}
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
        let x = Print(nm,x) in
        let s = 1 in
        {m=d.m; x=[x]; s=s}
    | Call(_) -> print_endline ("Call is showing up in check"); d
    | For(str, Range(a,b), lines) ->
        let a = eval d.m a in
        let b = eval d.m b in
        let _ = rangeIsValid a b in

        let a = getLit a in
        let b = getLit b in 
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
        let x = For(str, Range(Lit(a),Lit(b)), lines) in
        {m=d.m; x=[x]; s=0}
    | ModExpr(MD(out,_,_,_), _, _) -> 
        (*When ModExpr has no index, it just returns its first out*)
        let selected = 
            if (List.length out> 0)
            then snd(List.hd out)
            else "" in
        checkMod x d selected 
    | Noexpr -> d
    | a -> print_endline("missing case in checkvalidities: " ^ getBinExpr a ^ "DONE\n") ; d


and checkMod (ModExpr(MD(out,name,fms,exprs), args, par)) d selected= 
        let oldD = d in
        let _ = 
            if List.length fms = List.length args
            then ()
            else icERR name fms args in 

        let m = List.fold_left2 (tableFn oldD) StringMap.empty fms args in 
        let m = List.fold_left findRegs m exprs in
        let d = {m=m; x=[]; s=d.s} in

        let foldFn d line = 
                let d2 = check d line in
                {m=d2.m; x=d.x@d2.x; s=d2.s} in
        let d = List.fold_left foldFn d exprs in

            
            (*TODO this actually needs to be declared recursively for every binExpr but I don't feel like it rn*)
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

        let x = ModExpr(MD(out,name,fms,d.x),args, par) in
        {m=d.m; x=[x]; s=s}

and tableFn oldD m (fmX,nm) arg = 
     let argS = (check oldD arg).s in
     match fmX with
     | IntId(x) -> 
            let x = "*"^x in
            let m =
               if StringMap.mem x m
               then if (StringMap.find x m = argS)
                    then m
                    else let _ = p ("error") in m
                    (*TODO write this as a real error*)
               else StringMap.add x argS m in
            let m = StringMap.add nm argS m in
            m
     | x -> 
            let fmS = getLit(eval oldD.m fmX) in
            let _ =
                if (fmS = argS)
                then ()
                else tm_argERR arg argS nm fmS in
            let m = StringMap.add nm argS m in
            m

let semant hast =
    let d = {m=StringMap.empty; x=[]; s=0} in
    List.hd ((check d hast).x)
