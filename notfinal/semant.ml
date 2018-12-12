open Ast
open Printer
module StringMap = Map.Make(String)

exception InvalidAssignment of string
exception UndeclaredVar of string 
exception TypeMismatch of string
exception InvalidRange of string
exception InvalidCall of string

let getLit exp = (match exp with
       Lit(x) -> x
     | x -> p ("missed case: "^ Printer.getIntExpr x); 0)

(*Moving error messages out of the way, for readability*)
let uvERR name = raise (UndeclaredVar ("Variable \"" ^ name ^ "\" is not defined!"))

let tmERR op l r = raise(TypeMismatch ("You tried performing " ^ opToStr op ^ " on " ^ Printer.getBinExpr l ^" and "^Printer.getBinExpr r^" but these are of different sizes"))

let tm_assERR rval rtyp x a' b'= raise(TypeMismatch ("You tried assigning " ^ Printer.getBinExpr rval^ " of size " ^ string_of_int rtyp ^ " to " ^  x ^ " on the range " ^ string_of_int a' ^ "-" ^ string_of_int b' ^ " but these are of different sizes"))

let irERR x nm outs = raise (InvalidRange ("ERROR: You tried to access the "^string_of_int x^"th element of "^nm^" but it only has "^string_of_int (List.length outs)^" outputs!"))

let tm_fmERR arg fmVal fm x = raise(TypeMismatch ("You tried assigning argument " ^ Printer.getBinExpr arg^ " of size "^ string_of_int (fst fmVal) ^ " to formal " ^ fm ^ "<" ^ string_of_int x ^ "> but these are of different sizes"))

let tm_outERR out sz = raise(TypeMismatch ("The output call "^ snd out ^"<"^ string_of_int (getLit (fst out)) ^ ">"^ "does not match assignment of size "^ string_of_int sz)) 

let icERR name fms args= raise(InvalidCall ("Call to " ^ name ^ " with " ^ string_of_int (List.length args)
        ^ " arguments but " ^ name ^ " expects " ^ string_of_int (List.length fms) ^ " arguments."))

(*some utility fns*)
let printMap map name=
        print_endline ("\nprinting map (" ^ name ^ ")");
        StringMap.iter (fun k v -> print_string(k ^"<"^string_of_int v^">, ")) map;
        print_endline ("\ndone printing map");;

let lookup str map =
       StringMap.find str map


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

let rangeIsValid a b = 
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

let rec size map expr = 
        match expr with  
      Buslit(valz) -> (String.length valz -1, map, expr)
    | BoolId(name) -> 
        if StringMap.mem name map
        then (lookup name map, map, expr)
        (*TODO: Need a "badsearch" in case var hasn't been decl'd yet*)
        else uvERR name 
    | BoolBinop(l, op, r) -> 
        let ltyp = size map l in
        let rtyp = size map r in
        if (fst ltyp = fst rtyp)
        then ltyp
        else tmERR (B  op) l r
    | Unop(_, expr) -> (size map expr)
    | Assign(_, lval, rval, init) ->
        let _ = assignIsValid lval in
        let rtyp = fst(size map rval) in
        (match lval with
              BoolId(x) -> 
                      (rtyp, StringMap.add x rtyp map)
            | Index(BoolId(x), Range(a,b)) -> 
                let b' = getLit b in
                let a' = getLit a in
                if (rtyp = (b'-a'+1))
                then if StringMap.mem x map
                     then if (lookup x map < b'+1)
                          then (rtyp, StringMap.add x (b'+1) map)
                          else (rtyp, map)
                     else (rtyp, StringMap.add x (b'+1) map) 
                else tm_assERR rval rtyp x a' b'
            | x -> p ("Missed case: "^ Printer.getBinExpr x); (rtyp, map)
        )
    (*TODO note that harden will be weird and we actually need to account for this case*)
    | Index(ModExpr(MD(outs,nm,_,_),_,_), Range(a,b)) ->
         if (a = b)
         then match a with
             Lit(x) -> 
                 if (List.length outs > x)
                 then (x,map)
                 else irERR x nm outs
           | IntId(x) ->
                 if (List.exists (fun (_,a) -> a=x) outs)
                 then (getLit (fst(List.find (fun (_,a) -> a=x) outs)),map)
                 else raise(InvalidRange("ERROR: You tried to access "^x^" but "^nm^" has no such outputs!"))
         else raise(InvalidRange ("You can only access one output from a module at a time!"))
    | Index(expr, Range(a,b)) -> 
         let a' = getLit a in
         let b' = getLit b in
         let _ = rangeIsValid a' b' in
         let size = fst(size map expr) in
             if (size > b')
             then (b'-a'+1, map)
             else raise(TypeMismatch ("ERROR: You tried accessing a number too big"))
             (*TODO write a better message*)
             (*TODO I think this accounts for every case, but haven't checked, run tests*)
    | Print(_, exp) -> size map exp
    | Call(_) -> print_endline ("something is way wrong. Call is showing up in size"); (0,map)
    | For(str, Range(Lit(a),Lit(b)), lines) ->
         let lines = List.map (Noloop2.replace str b) lines in
         let maps = List.map (size map) lines in
         let _ = rangeIsValid a b in
         let f k v1 v2 = Some v1 in
         let helper outMap (_,map) = StringMap.union f outMap map in 
         let newMap = List.fold_left helper StringMap.empty maps in
         (0, newMap)
    (*Indexing has to check whether it contains a ModExpr. If so, it acts differenty*)
    (*When ModExpr has no index, it just returns its first out*)
    | ModExpr(MD(out,name,fms,exprs), args, _) -> 
        if List.length fms = List.length args
        then
            let oldMap = map in 
            let map2fn map ((Lit(x)),fm) arg =
                    let fmVal = size map arg in
                    if (fst fmVal = x)
                    then (0, map)
                    else tm_argERR arg fmVal fm x in
            let checkArgs = List.map2 (map2fn map) fms args in

            let regs = List.fold_left findRegs map exprs in
            let fold2fn (_,m) ((Lit(x)), fm) arg = (0, StringMap.add fm x m) in
            let initVarTable = List.fold_left2 fold2fn (0,regs) fms args in
            let foldfn (_,b) expr = (size b) expr in
            let result = List.fold_left foldfn initVarTable (exprs) in
            let finalMap = snd result in 
            let getLit (Lit(x),_) = x in
            
            (*TODO this actually needs to be declared recursively for every binExpr but I don't feel like it rn*)
            let checkOut out = 
                if StringMap.mem (snd out) finalMap 
                then 
                    let sz = lookup (snd out) finalMap in
                    if (sz = getLit out)
                    then ()
                    else tm_outERR out sz 
                else raise(UndeclaredVar ("The output call "^ snd out^ " is never defined")) in
            let _ = List.iter checkOut (*cart @Amazon*) out in
            if (List.length out = 0)
            then (0, map)
            else (getLit (List.hd out), map)
        else icERR name fms args
    | a -> print_endline("missing case in checkvalidities: " ^ getBinExpr a ^ "DONE\n") ; (0,map)

let check hast =
    size StringMap.empty hast
