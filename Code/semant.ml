open Ast
open Sast
module StringMap = Map.Make(String)


let rec evalInt = function
 | Lit(x) -> x
 | IntId(x) -> 1 (*have to have evalInt x here, but really evalInt x depends on a values table (and will be hard to implement)*)
 | IntBinop(a,Add,b) -> evalInt a + evalInt b
 | IntBinop(a,Sub,b) -> evalInt a - evalInt b

let evalBind (a,b) = (string_of_int (evalInt a), b)

let hastToSast hast = [{outlist = [(1, "out1")]; name = "module name"; formals = []; locals = []; linelist = []}]

let generateLocals linelist = []

let astToHast ast =
let hardenBind (expr, name) = (evalInt expr, name) in
let hardenBinds blist = List.map hardenBind blist in
let mdToHmd (o, n, f, ll) = 
           {outlistH =  hardenBinds o; 
            name = n;
            formals = hardenBinds f;
            locals = generateLocals ll;
            linelist = ll} in
List.map mdToHmd ast

let ssast sast = ([(1, "input1")], [], [(1, "output1")])

let check x = ssast (hastToSast (astToHast x))
