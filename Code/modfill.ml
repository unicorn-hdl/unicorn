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
let rec runThroughLines d par m m2 = List.rev (List.map actOnline (modzIntoTuples d par m m2))
(*TODO change this to fold left, so as to pass on changes to m2*)
        (*Perhaps not necessary? Seems like it's working ok as is. Run tests.*)

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
                    ModExpr(Module_decl(a,b,c, (runThroughLines d par m m2)), args, par)
               else raise( MissingFunction ("Module " ^ name ^ 
               " not found! Make sure module is declared."))
    | ModExpr(Module_decl(a,b,c,d), args, parent) -> 
            let _ = print_endline ("Something's wrong! modExpr called in modfill") in
            let this = ModExpr(Module_decl(a,b,c,d), args, None) in
            if StringMap.find b m2
            then ModExpr(Module_decl(a,b,c,d), args, Some this)
            else ModExpr(Module_decl(a,b,c, runThroughLines d par m m2), args, Some this)
            (*TODO actually make updates to m2*)
            (*TODO it seems like parent is never actually used. If unecessary, trash it*)
            (*It also seems like ModExpr should never be called. Check that it isn't*)
    | Noexpr -> Noexpr
    | a -> print_endline("ERROR: Case not found!"); a

let rec setPars (line,par, m, m2) = match line with
    | Buslit(x) -> Buslit(x)
    | BoolId(x) -> BoolId(x)
    | BoolBinop(l, op, r) -> BoolBinop(setPars (l,par, m, m2), op, setPars (r,par,m, m2))
    | Unop(op, exp) -> Unop(op, setPars (exp,par,m,m2))
    | Assign(a, e1, e2, b) -> Assign(a, setPars (e1,par,m,m2), setPars (e2,par,m,m2), b)
    | Index(e, r) -> Index(setPars (e,par,m, m2), r)
    | Print(s, e) -> Print(s, setPars (e,par,m, m2))
    | For(s, r, e) -> For(s, r, List.map (fun x -> setPars(x,par,m,m2)) e)
    | Call(name, args) -> 
               let _ = print_endline ("Something's wrong! Call called in modfill setPars") in
               Call(name,args)
    | ModExpr(Module_decl(a,b,c,d), args, parent) -> 
            if (par = None)
            then ModExpr(Module_decl(a,b,c, List.map (fun x -> setPars(x,Some line,m,m2)) d), args, None)
            else ModExpr(Module_decl(a,b,c, List.map (fun x -> setPars(x,Some line,m,m2)) d), args, par )
    | Noexpr -> Noexpr
    | a -> print_endline("ERROR: Case not found!"); a


(*Helper method for fillHelper. Replaces d in some mod with d' where d' is lines where calls are replaced with mods*)
let replaceCalls (Module_decl(a, b, c, d), par, m, m2) =
                        ((Module_decl(a, b, c, (runThroughLines d par m m2))), m, m2)


(*create map that links module names to the modules themselves*)
let populateMap map (Module_decl(a,b,c,d)) = StringMap.add b (Module_decl(a,b,c,d)) map
let createMapz mdlist = List.fold_left populateMap StringMap.empty mdlist;;

(*Make a string map that keeps track of whether a module has been "decompressed"*)
let popIsFilledMap map (Module_decl(a,b,c,d)) = StringMap.add b false map
let makeIsFilledMap mdlist = List.fold_left popIsFilledMap StringMap.empty mdlist;;

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
(*
let fillMap = makeIsFilledMap mdlistEx;;
let theMap = createMapz mdlistEx;;
*)
let main nameMap = if StringMap.mem "main" nameMap 
    then StringMap.find "main" nameMap
    else raise(MissingFunction "There is no main function. Please create a main");;
    (*
    else par;;
*)
let fillHelper mdlist nameMap fillMap = replaceCalls ((main nameMap), None, nameMap, fillMap);;
let genFill mdlist nameMap fillMap = (fun (a,b,c)-> a) (fillHelper mdlist nameMap fillMap);;

(*~fn called in unic~*)
(*mdlist -> md*)
let fill mdlist = 
        let filledMap =  fillHelper mdlist (createMapz mdlist) (makeIsFilledMap mdlist) in
        let fst (a,_,_) = a in
        let snd (_,b,_) = b in
        let thd (_,_,c) = c in
        setPars (ModExpr( (fst filledMap),[],None ), None, snd(filledMap), thd(filledMap))
(*~fn called in unic~*)

