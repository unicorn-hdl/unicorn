open Ast
open Printer
module StringMap = Map.Make(String)
exception MissingFunction of string 
let modzIntoTuples d m m2 = List.map (fun d-> (d,m,m2)) d
let fstM (MD(a,b,c,d)) = a
let sndM (MD(a,b,c,d)) = b
let thdM (MD(a,b,c,d)) = c
let fthM (MD(a,b,c,d)) = d
let toString (MD(a,b,c,d)) = b ^ "\n" ^ toStringBinExprlist "" d

(*replace calls in a mod with modules*)
let rec runThroughLines d m m2 = List.rev (List.map actOnline (modzIntoTuples d m m2))
(*TODO change this to fold left, so as to pass on changes to m2*)
        (*Perhaps not necessary? Seems like it's working ok as is. Run tests.*)

(*replace calls in a line with the module*)
and actOnline (line, m, m2) = 
        (*TODO use this to see if we can kill par*)
        (*
    let _ = (match par with
        None -> (*p ("none")*) ()
       | _ -> p "something!!!!") in
*)
        match line with 
    | Buslit(x) -> Buslit(x)
    | BoolId(x) -> BoolId(x)
    | BoolBinop(l, op, r) -> BoolBinop(actOnline (l, m, m2), op, actOnline (r,m, m2))
    | Unop(op, exp) -> Unop(op, actOnline (exp,m,m2))
    | Assign(a, e1, e2, b) -> Assign(a, actOnline (e1,m,m2), actOnline (e2,m,m2), b)
    | Index(e, r) -> Index(actOnline (e,m, m2), r)
    | Print(s, e) -> Print(s, actOnline (e,m, m2))
    | For(s, r, e) -> For(s, r, runThroughLines e m m2)
    | Call(name, args) -> 
               let args = List.map (fun x -> actOnline(x, m, m2)) args in
               if StringMap.mem name m
               then
                    let modz = StringMap.find name m in
                    let stringOf = toString modz in
                    let a = fstM modz in
                    let b = sndM modz in
                    let c = thdM modz in
                    let d = fthM modz in
                    ModExpr(MD(a,b,c, (runThroughLines d m m2)), args)
               else raise( MissingFunction ("Module " ^ name ^ 
               " not found! Make sure module is declared."))
    | ModExpr(MD(a,b,c,d), args) -> 
            let _ = print_endline ("Something's wrong! modExpr called in modfill") in
            let this = ModExpr(MD(a,b,c,d), args) in
            if StringMap.find b m2
            then ModExpr(MD(a,b,c,d), args)
            else ModExpr(MD(a,b,c, runThroughLines d m m2), args)
            (*TODO actually make updates to m2*)
    | Noexpr -> Noexpr
    | a -> print_endline("ERROR: Case not found!"); a

    (*
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
    | ModExpr(MD(a,b,c,d), args, parent) -> 
            if (par = None)
            then ModExpr(MD(a,b,c, List.map (fun x -> setPars(x,Some line,m,m2)) d), args, None)
            else ModExpr(MD(a,b,c, List.map (fun x -> setPars(x,Some line,m,m2)) d), args, par )
    | Noexpr -> Noexpr
    | a -> print_endline("ERROR: Case not found!"); a
    *)


(*Helper method for fillHelper. Replaces d in some mod with d' where d' is lines where calls are replaced with mods*)
let replaceCalls (MD(a, b, c, d), m, m2) =
                        ((MD(a, b, c, (runThroughLines d m m2))), m, m2)


(*create map that links module names to the modules themselves*)
let populateMap map (MD(a,b,c,d)) = StringMap.add b (MD(a,b,c,d)) map
let createMapz mdlist = List.fold_left populateMap StringMap.empty mdlist;;

(*Make a string map that keeps track of whether a module has been "decompressed"*)
let popIsFilledMap map (MD(a,b,c,d)) = StringMap.add b false map
let makeIsFilledMap mdlist = List.fold_left popIsFilledMap StringMap.empty mdlist;;

(*print stuff*) 
(*
let spitOut = fillHelper mdlistEx theMap fillMap;;
*)
let toString (MD(a,b,c,d)) = b ^ "\n" ^ toStringBinExprlist "" d
let printx (x,y,z) = print_endline(toString x)
let printz x = printx x;;

let printMapEl key v = print_endline(key ^ ":\n " ^ (toString v));;
let printMap m = StringMap.iter printMapEl m;;

let printMapEl2 key v = print_endline(key ^ ": " ^ (string_of_bool v));;
let printMap2 m = StringMap. iter printMapEl2 m;;

let main nameMap = if StringMap.mem "main" nameMap 
    then StringMap.find "main" nameMap
    else raise(MissingFunction "There is no main function. Please create a main");;
    (*
    else par;;
*)
let fillHelper mdlist nameMap fillMap = replaceCalls ((main nameMap), nameMap, fillMap);;
let genFill mdlist nameMap fillMap = (fun (a,b,c)-> a) (fillHelper mdlist nameMap fillMap);;

(*~fn called in unic~*)
(*mdlist -> md*)
let fill mdlist = 
        let filledMap =  fillHelper mdlist (createMapz mdlist) (makeIsFilledMap mdlist) in
        let fst (a,_,_) = a in
        let snd (_,b,_) = b in
        let thd (_,_,c) = c in
        let mainDec = fst filledMap in
        let mainCall = ModExpr(mainDec, Io.getMainArgs mainDec) in 
        let supermainDec = MD([],"~",[],List.rev (mainCall::(Io.makeVars mainDec))) in
        ModExpr(supermainDec, [])
(*~fn called in unic~*)

