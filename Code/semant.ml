open Ast
open Printer
module StringMap = Map.Make(String)

exception InvalidAssignment of string
exception UndeclaredVar of string 
exception TypeMismatch of string
exception InvalidRange of string
exception InvalidCall of string

let printMap map name=
        print_endline ("\nprinting map (" ^ name ^ ")");
        StringMap.iter (fun k v -> print_string(k ^"<"^string_of_int v^">, ")) map;
        print_endline ("\ndone printing map");;


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

let rec checkValidity map expr = match expr with  
      Buslit(valz) -> (String.length valz -1, map)
    | BoolId(name) -> 
        if StringMap.mem name map
        then (StringMap.find name map, map)
        else raise (UndeclaredVar ("Variable \"" ^ name ^ "\" is not defined!"))
    | BoolBinop(l, op, r) -> 
        let ltyp = checkValidity map l in
        let rtyp = checkValidity map r in
        if (fst ltyp = fst rtyp)
        then (ltyp)
        else raise(TypeMismatch 
            ("You tried performing " ^ opToStr (B(op)) ^ " on " ^ Printer.getBinExpr l
            ^ " and " ^ Printer.getBinExpr r ^ " but these are of different sizes"))
    | Unop(_, expr) -> (checkValidity map expr)
    | Assign(_, lval, rval, init) ->
    (*TODO init should become a string so that we can check this stuff correctly*)
        let _ = assignIsValid lval in
        let getLit exp = (match exp with
               Lit(x) -> x
             | x -> print_endline("missed case: "^ Printer.getIntExpr x); 0) in
        let rtyp = fst(checkValidity map rval) in
        (match lval with
              BoolId(x) -> 
                      (rtyp, StringMap.add x rtyp map)
            | Index(BoolId(x), Range(a,b)) -> 
                let b' = getLit b in
                let a' = getLit a in
                if (rtyp = (b'-a'+1))
                then 
                    if StringMap.mem x map
                    then
                        if StringMap.find x map < b'+1
                        then (rtyp, StringMap.add x (b'+1) map)
                        else (rtyp, map)
                    else (rtyp, StringMap.add x (b'+1) map) 
                else raise(TypeMismatch 
                ("You tried assigning " ^ Printer.getBinExpr rval
                ^ " of size " ^ string_of_int rtyp ^ " to " ^  x 
                ^ " on the range " ^ string_of_int a' ^ "-" ^ 
                string_of_int b' ^ " but these are of different sizes"))
            | x -> print_endline("Missed case: "^ Printer.getBinExpr x); (rtyp, map)
        )
    (*TODO note that harden will be weird and we actually need to account for this case*)
    | Index(expr, Range(a,b)) -> 
            let getLit exp = (match exp with
                  Lit(x) -> x
                | x -> print_endline("missed case: "^ Printer.getIntExpr x); 0) in
            let a' = getLit a in
            let b' = getLit b in
            let _ = rangeIsValid a' b' in
            let size = fst(checkValidity map expr) in
                if (size > b')
                then (b'-a'+1, map)
                else raise(TypeMismatch
                ("You tried accessing a number too big"))
                (*TODO write a better message*)
                (*TODO I think this accounts for every case, but haven't checked, run tests*)

(*I made this whole mess and then realized that this whole thing can (and should be taken care of in harden. Just check for out names in harden and return the appropriate ranges

            ModExpr(Module_decl(outs,b,c,d), args, par) -> 
                let isLit = function
                            Lit(x) -> true
                            _ -> false
                in
                if (isLit a && isLit b)
                then 
                    let getLit (Lit(x)) = x in
                    let a' = getLit a in
                    let b' = getLit b in
                    let _ = rangeIsValid a' b' in
                    if (a' = b')
                    then 
                    else raise(InvalidRange "You are trying to access mutiple 
                     outputs of module " ^ b ^ "in the same call.")
                else
*)
    | Print(_, exp) -> checkValidity map exp
    | Call(_) -> print_endline ("something is way wrong. Call is showing up in checkValidity");
                 (0,map)
    | For(str, Range(a,b), lines) ->
         List.map (checkValidity map) lines;
         let getLit (Lit(x)) = x in
         let a' = getLit a in
         let b' = getLit b in
         let _ = rangeIsValid a' b' in
         (*TODO need to include actual checking that compares indices to internal vals*)
         (0, map)

    (*Indexing has to check whether it contains a ModExpr. If so, it acts differenty*)
    (*When ModExpr has no index, it just returns its first out*)
    | ModExpr(Module_decl(out,name,fms,exprs), args, _) -> 
        if List.length fms = List.length args
        then
            let _ = print_endline ("checking mod " ^ name) in
            let _ = print_endline ("args: " ^ Printer.toStringBinExprlist args) in
            let oldMap = map in 
            let map2fn map ((Lit(x)),fm) arg =
                    let fmVal = checkValidity map arg in
                    if (fst fmVal = x)
                    then (0, map)
                    else raise(TypeMismatch 
                    ("You tried assigning argument " ^ Printer.getBinExpr arg^ " of size "^ string_of_int (fst fmVal)
                    ^ " to formal " ^ fm ^ "<" ^ string_of_int x ^ "> but these are of different sizes")) in
            let checkArgs = List.map2 (map2fn map) fms args in
            let fold2fn (_,m) ((Lit(x)), fm) arg = (0, StringMap.add fm x m) in
            let initVarTable = List.fold_left2 fold2fn (0,StringMap.empty) fms args in
            let _ = printMap (snd initVarTable) (name^"#init#") in
            let foldfn (_,b) expr = (checkValidity b) expr in
            let result = List.fold_left foldfn initVarTable (exprs) in
            let finalMap = snd result in 
            let getLit (Lit(x),_) = x in
            
            (*TODO this actually needs to be declared recursively, for every binExpr but I don't feel like it rn*)
            let listGen strmp expr = match expr with
                 Assign(_, lval, rval, _) ->(match lval with
                      BoolId(x) -> StringMap.add x (fst(checkValidity finalMap rval)) strmp
                    | Index(BoolId(x),_) -> StringMap.add x (fst(checkValidity finalMap rval)) strmp
                    | a -> strmp)
               | a -> strmp in
            let outVarMap = List.fold_left listGen StringMap.empty exprs in
            let _ = outVarMap in
            let checkOut outVarMap out = 
                    if StringMap.mem (snd out) outVarMap
                        then 
                            let sz = StringMap.find (snd out) outVarMap in
                            if (sz = getLit out)
                            then ()
                            else raise(TypeMismatch ("The output call "^ snd out ^"<"^ string_of_int (getLit out)^ ">"^
                            "does not match assignment of size "^ string_of_int sz)) 
                        else raise(UndeclaredVar ("The output call "^ snd out^ " is never defined")) in
            let _ = List.iter (checkOut (*cart @Amazon*) outVarMap) out in
            if (List.length out = 0)
            then (0, map)
            else (getLit (List.hd out), map)

        else raise(InvalidCall ("Call to " ^ name ^ " with " ^ string_of_int (List.length args)
        ^ " arguments but " ^ name ^ " expects " ^ string_of_int (List.length fms) ^ " arguments."))
            
    | a -> print_endline("missing case in checkvalidities: " ^ getBinExpr a ^ "DONE\n") ; (0,map)


let check (Module_decl(out, nm, fm, binEx))= 
    let hast = Module_decl(out, nm, fm, binEx) in
    checkValidity StringMap.empty (ModExpr(hast,[], emptyMod));;
