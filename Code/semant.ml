open Ast
open Sast
module StringMap = Map.Make(String)

type hardBind = int * string
type hardSmd = {outlistH: hardBind list; name: string; formals: hardBind list; locals: hardBind list; lineList: sbinExpr list}

let rec evalInt = function
 | Lit(x) -> x
 | IntId(x) -> 1 (*have to have evalInt x here, but really evalInt x depends on a values table (and will be hard to implement)*)
 | IntBinop(a,Add,b) -> evalInt a + evalInt b
 | IntBinop(a,Sub,b) -> evalInt a - evalInt b

let evalBind (a,b) = (string_of_int (evalInt a), b)

let astTosast ast = [{outlist = [(Lit(1), "out1")]; name = "module name"; formals = []; locals = []; lineList = []}] (*Obviously this has to made into a real thing TODO*)

let harden sast =
let hardenBind (expr, name) = (evalInt expr, name) in
let hardenBinds blist = List.map hardenBind blist in
let fn {outlist = o; name = n; formals = f; locals = l; lineList = ll } = 
           {outlistH =  hardenBinds o; 
            name = n;
            formals = hardenBinds f;
            locals = hardenBinds l;
            lineList = ll} in
List.map fn sast

let check x = harden (astTosast x)
