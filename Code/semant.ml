open Ast
open Printer
module StringMap = Map.Make(String)

exception InvalidAssignment of string
exception UndeclaredVar of string 
exception TypeMismatch of string
exception InvalidRange of string

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
        print_endline ("printing map (" ^ name ^ ")");
        StringMap.iter (fun k _ -> print_string(k ^ ", ")) map;
        print_endline ("");
        if StringMap.mem name map
        then (StringMap.find name map, map)
        else raise (UndeclaredVar ("Variable \"" ^ name ^ "\" is not defined!"))
    | BoolBinop(l, op, r) -> 
        let ltyp = checkValidity map l in
        let rtyp = checkValidity map r in
        if (ltyp = rtyp)
        then (ltyp)
        else raise(TypeMismatch 
            ("You tried performing " ^ bOpToStr op ^ " on " ^ Printer.getBinExpr l
            ^ " and " ^ Printer.getBinExpr r ^ " but these are of different sizes"))
    | Unop(_, expr) -> (checkValidity map expr)
    | Assign(_, lval, rval, init) ->
    (*TODO init should become a string so that we can check this stuff correctly*)
        assignIsValid lval;
        let getLit (Lit(x)) = x in
        let rtyp = fst(checkValidity map rval) in
        (match lval with
              BoolId(x) -> 
                      print_endline ("printing in assign");
                      let ans = (rtyp, StringMap.add x rtyp map)
                      in let _ = StringMap.iter (fun k v -> print_string(k ^ "["^ string_of_int v ^ "], ")) (snd ans)
                      in let _ = print_endline ("done printing")
                      in ans 
            | Index(BoolId(x), Range(a,b)) -> 
                print_endline("in index");
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
        )
    | Index(expr, Range(a,b)) -> 
        (match expr with
            (*TODO note that harden will be weird and we actually need to account for this case*)
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

            | e -> 
                let getLit (Lit(x)) = x in
                let a' = getLit a in
                let b' = getLit b in
                let _ = rangeIsValid a' b' in
                let size = fst(checkValidity map e) in
                    if (size<= b')
                    then (b'-a'+1, map)
                    else raise(TypeMismatch
                    ("You tried accessing a number too big"))
                    (*TODO write a better message*)
                    (*TODO I think this accounts for every case, but haven't checked, run tests*)
        )        
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
    | ModExpr(Module_decl(out,_,_,exprs), _, _) -> 
        let foldfn (_,b) expr = (checkValidity b) expr in
        List.fold_left foldfn (0,StringMap.empty) (List.rev exprs);
        let getLit (Lit(x),_) = x in
        if (List.length out = 0)
        then (0, map)
        else (getLit (List.hd out), map)
    | a -> print_endline("missing case in checkvalidities: " ^ getBinExpr a ^ "DONE\n") ; (0,map)

    (*
let buildSast (Module_decl(outs, name, forms, lines))= 
        {outlist = List.map unwrap outs;
         name = name;
         formals = List.map unwrap forms;
         linelist = List.map semantify lines}
*)

let initVarTable hast = StringMap.empty

let check (Module_decl(out, nm, fm, binEx))= 
    let hast = Module_decl(out, nm, fm, binEx) in
    (*
    let sast = buildSast hast in
*)
    let table = initVarTable hast in
    checkValidity table (ModExpr(hast,[], emptyMod));;
