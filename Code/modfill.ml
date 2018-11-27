open Ast
open Printer
module StringMap = Map.Make(String)

let modzIntoTuples d m = List.map (fun d-> (d,m)) d
let rec runThroughLines d m = List.map actOnline (modzIntoTuples d m)

and actOnline (newmodlist, m) = match newmodlist with 
    | Buslit(x) -> Buslit(x)
    | BoolId(x) -> BoolId(x)
    | BoolBinop(l, op, r) -> BoolBinop(actOnline (l,m), op, actOnline (r,m))
    | Unop(op, exp) -> Unop(op, actOnline (exp,m))
    | Assign(a, e1, e2, b) -> Assign(a, actOnline (e1,m), actOnline (e2,m), b)
    | Index(e, r) -> Index(actOnline (e,m), r)
    | Print(s, e) -> Print(s, actOnline (e,m))
    (*
    | For(s, r, e) -> For(s, r, runThroughLines e m)
*)
    | Call(name, args) -> ModExpr(StringMap.find name m)
    | ModExpr(x) -> ModExpr(x)
    | Noexpr -> Noexpr


let populateMap m (Module_decl(a,b,c,d), e) = StringMap.add b (Module_decl(a,b,c,d)) m

let replaceCalls (Module_decl(a, b, c, d), m) =
                        ((Module_decl(a, b, c, runThroughLines d m)), m)

let callx x = ( Call(x, []) )
let modA = Module_decl([], "modA", [], [])
let bExprs = 
        [Print("printname", callx "modA")]
let modB = Module_decl([], "modB", [], bExprs)
let e = StringMap.empty

let mdlistEx = [modA;
                modB
               ]

(*md list-> md list*)
let createMapz mdlist = List.fold_left populateMap StringMap.empty mdlist;;
let call mdlist m= List.map replaceCalls (modzIntoTuples mdlist m);;

let theMap = createMapz (modzIntoTuples mdlistEx StringMap.empty);;
let x = call mdlistEx theMap;;

let toString (Module_decl(a,b,c,d)) = b ^ "\n" ^ toStringBinExprlist d
let printx (x,y) = print_endline(toString x)
let printz x = List.iter printx x;;
let printMapEl key v = print_endline(key ^ ": " ^ (toString v))
let printMap m = StringMap.iter printMapEl m;;

printz x;;
(*
printMap theMap 
*)
