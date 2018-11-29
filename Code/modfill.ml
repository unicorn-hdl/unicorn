open Ast
open Printer
module StringMap = Map.Make(String)

exception MissingFunction of string

let modzIntoTuples d par m m2 = List.map (fun d-> (d,par, m,m2)) d
let fstM (Module_decl(a,b,c,d)) = a
let sndM (Module_decl(a,b,c,d)) = b
let thdM (Module_decl(a,b,c,d)) = c
let fthM (Module_decl(a,b,c,d)) = d
let toString (Module_decl(a,b,c,d)) = b ^ "\n" ^ toStringBinExprlist d

(*replace calls in a mod with modules*)
let rec runThroughLines d par m m2 = List.map actOnline (modzIntoTuples d par m m2)

(*replace calls in a line with the module*)
and actOnline (line, par, m, m2) = match line with 
    | Buslit(x) -> Buslit(x)
    | BoolId(x) -> BoolId(x)
    | BoolBinop(l, op, r) -> BoolBinop(actOnline (l,par, m, m2), op, actOnline (r,par,m, m2))
    | Unop(op, exp) -> Unop(op, actOnline (exp,par,m,m2))
    | Assign(a, e1, e2, b) -> Assign(a, actOnline (e1,par,m,m2), actOnline (e2,par,m,m2), b)
    | Index(e, r) -> Index(actOnline (e,par,m, m2), r)
    | Print(s, e) -> Print(s, actOnline (e,par,m, m2))
    | For(s, r, e) -> For(s, r, runThroughLines e par m m2)
    | Call(name, args) -> 
               if StringMap.mem name m
               then
                    let modz = StringMap.find name m in
                    let stringOf = toString modz in
                    let a = fstM modz in
                    let b = sndM modz in
                    let c = thdM modz in
                    let d = fthM modz in
                    ModExpr(Module_decl(a,b,c, runThroughLines d par m m2), args, par)
               else raise( MissingFunction ("Module " ^ name ^ 
               " not found! Make sure module is declared."))
(*
Call(name, args)
*)
    | ModExpr(Module_decl(a,b,c,d), args, parent) -> if StringMap.find b m2
            then ModExpr(Module_decl(a,b,c,d), args, parent)
            else ModExpr(Module_decl(a,b,c, runThroughLines d par m m2), args, parent)
    | Noexpr -> Noexpr
    | a -> print_endline("ERROR: Case not found!"); a

(*Helper method for fillHelper. Replaces d in some with d' where d' is lines where calls are replaced with mods*)
let replaceCalls (Module_decl(a, b, c, d), par, m, m2) =
                        ((Module_decl(a, b, c, runThroughLines d par m m2)), m, m2)

(*create an example ast*)
let callx x = ( Call(x, []) )
let modA = Module_decl([], "modA", [], [])
let bExprs = 
        [Print("printname", callx "modA")]
let modB = Module_decl([], "modB", [], bExprs)
let e = StringMap.empty

let mdlistEx = [modA;
                modB
               ]
;;

(*create map that links module names to the modules themselves*)
let populateMap map (Module_decl(a,b,c,d)) = StringMap.add b (Module_decl(a,b,c,d)) map
let createMapz mdlist = List.fold_left populateMap e mdlist;;

(*Make a string map that keeps track of whether a module has been "decompressed"*)
let popIsFilledMap map (Module_decl(a,b,c,d)) = StringMap.add b false map
let makeIsFilledMap mdlist = List.fold_left popIsFilledMap e mdlist;;

(*print stuff*) 
(*
let spitOut = fillHelper mdlistEx theMap fillMap;;
*)
let toString (Module_decl(a,b,c,d)) = b ^ "\n" ^ toStringBinExprlist d
let printx (x,y,z) = print_endline(toString x)
let printz x = printx x;;

let printMapEl key v = print_endline(key ^ ":\n " ^ (toString v));;
let printMap m = StringMap.iter printMapEl m;;

let printMapEl2 key v = print_endline(key ^ ": " ^ (string_of_bool v));;
let printMap2 m = StringMap. iter printMapEl2 m;;

(*
printz spitOut;;

print_endline("PRINTING THE FILL MAP");;
printMap2 (makeIsFilledMap mdlistEx);;

printMap theMap 
*)

(*general calls*)
let fillMap = makeIsFilledMap mdlistEx;;
let theMap = createMapz mdlistEx;;
let par = Module_decl([],"",[],[]);;
let main nameMap = if StringMap.mem "main" nameMap 
    then StringMap.find "main" nameMap
    else raise(MissingFunction "There is no main function. Please create a main");;
    (*
    else par;;
*)
let fillHelper mdlist nameMap fillMap = replaceCalls ((main nameMap), par, nameMap, fillMap);;
let genFill mdlist nameMap fillMap = (fun (a,b,c)-> a) (fillHelper mdlist nameMap fillMap);;

(*~fn called in unic~*)
(*mdlist -> md*)
let fill mdlist = genFill mdlist (createMapz mdlist) (makeIsFilledMap mdlist);;
(*~fn called in unic~*)

