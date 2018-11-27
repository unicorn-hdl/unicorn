open Ast
module StringMap = Map.Make(String)

let rec actOnline (newmodlist, m) = match newmodlist with 
    | Call(name, args) -> ModExpr(StringMap.find name m)

let modzIntoTuples d m = List.map (fun d-> (d,m)) d
let runThroughLines d m = List.map actOnline (modzIntoTuples d m)

let populateMap m (Module_decl(a,b,c,d), e) = StringMap.add b (Module_decl(a,b,c,d)) m

let replaceCalls ((Module_decl(a, b, c, d)), m) =
                        ((Module_decl(a, b, c, runThroughLines d m)), m)

let callx x = (Call(x, []))
let modx x = (Module_decl([], x, [], []))
let e = StringMap.empty

let mdlistEx = [((modx "modA"), e);
                ((modx "modB"), e)
               ]

(*
let mdlistEx = [(1,2); (3,2); (5,12)]
*)

(*md list-> md list*)
let createMapz mdlist = List.fold_left populateMap StringMap.empty mdlist
let call mdlist = List.map replaceCalls mdlist

let theMap = createMapz mdlistEx
let x = call mdlistEx

let toString (Module_decl(a,b,c,d)) = b
let printx (x,y) = print_endline(toString x)
let printz x = List.iter printx x;;
let printMapEl key v = print_endline(key ^ ": " ^ (toString v))
let printMap m = StringMap.iter printMapEl m;;

printz x;;
printMap theMap 
