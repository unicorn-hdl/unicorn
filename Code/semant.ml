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

let assignIsValid (Assign(a, lval, c, d)) = match lval with
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
      Buslit(valz) -> String.length valz
    | BoolId(name) -> if StringMap.mem name map
        then StringMap.find name map
        else raise (UndeclaredVar ("Variable \"" ^ name ^ "\" is not defined!"))
    | BoolBinop(l, op, r) -> 
        let ltyp = checkValidity map l in
        let rtyp = checkValidity map r in
        if (ltyp = rtyp)
        then ltyp
        else raise(TypeMismatch 
            ("You tried performing " ^ bOpToStr op ^ " on " ^ Printer.getBinExpr l
            ^ " and " ^ Printer.getBinExpr r ^ " but these are of different sizes"))
    


    (*Indexing has to check whether it contains a ModExpr. If so, it acts differenty*)
    (*When ModExpr has no index, it just returns its first out*)
    | ModExpr(Module_decl(out,nm,fm,exprs), args, par) -> 
                    List.map (checkValidity map) exprs;
                    let getLit (Lit(x),str) = x in
                    if (List.length out = 0)
                    then 0
                    else getLit (List.hd out)
    | a -> print_endline("missing case in checkvalidities: " ^ getBinExpr a ^ "DONE\n") ; 0

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
