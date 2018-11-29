open Ast
open Sast
open Printer
module StringMap = Map.Make(String)

exception InvalidAssignment of string
exception UndeclaredVar of string 
exception TypeMismatch of string

let rec evalInt = function
 | Lit(x) -> x
 | IntId(x) -> 1 (*have to have evalInt x here, but really evalInt x depends on a values table (and will be hard to implement)*)
 | IntBinop(a,Add,b) -> evalInt a + evalInt b
 | IntBinop(a,Sub,b) -> evalInt a - evalInt b

let evalBind (a,b) = (string_of_int (evalInt a), b)

let assignIsValid lval = match lval with
      BoolId(x) -> () (*valid*)
    | Index(BoolId(x), a) -> () (*valid*)
    | x -> raise(InvalidAssignment("\"" ^ Printer.getBinExpr x ^ "\" may not be assigned to"))

    (*
let rec semantify = function
      Buslit(x) -> (String.length x, SBuslit(x))
    | BoolId(x) -> (0, SBoolId(x)) (*TODO: need to do this for real*)

let unwrap (a,b) = match a with
      Lit (x) -> (x,b)
    | _ -> print_endline("something is wrong in semant"); (0,b)  
*)

let rec checkValidity map expr = match expr with  
      Buslit(valz) -> (String.length valz, map)
    | BoolId(name) -> 
        print_endline ("printing map (" ^ name ^ ")");
        StringMap.iter (fun k v -> print_string(k ^ ", ")) map;
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
    | Unop(op, expr) -> (checkValidity map expr)
    | Assign(isReg, lval, rval, init) ->
        print_endline("in here");
        assignIsValid lval;
        let getLit (Lit(x)) = x in
        let rtyp = fst(checkValidity map rval) in
        (match lval with
              BoolId(x) -> 
                      print_endline ("printing in assign");
                      let x = (rtyp, StringMap.add x rtyp map)
                      in let _ = StringMap.iter (fun k v -> print_string(k ^ ", ")) (snd x)
                      in let _ = print_endline ("done printing")
                      in x 
            | Index(BoolId(x), Range(a,b)) -> 
                print_endline("in index");
                let b' = getLit b in
                let a' = getLit a in
                if (rtyp = (b'-a'+1))
                then 
                    if StringMap.mem x map
                    then
                        if StringMap.find x map < b'
                        then (rtyp, StringMap.add x b' map)
                        else (rtyp, map)
                    else (rtyp, StringMap.add x b' map) 
                else raise(TypeMismatch 
                ("You tried assigning " ^ Printer.getBinExpr rval
                ^ " of size " ^ string_of_int rtyp ^ " to " ^ x 
                ^ "on the range " ^ string_of_int a' ^ "-" ^ 
                string_of_int b' ^ " but these are of different sizes"))
        )

    (*Indexing has to check whether it contains a ModExpr. If so, it acts differenty*)
    (*When ModExpr has no index, it just returns its first out*)
    | ModExpr(Module_decl(out,nm,fm,exprs), args, par) -> 
        let foldfn (a,b) expr = (checkValidity b) expr in
        List.fold_left foldfn (0,StringMap.empty) (List.rev exprs);
        let getLit (Lit(x),str) = x in
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
