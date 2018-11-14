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
let mdToHmd = function | Module_decl(o, n, f, ll) -> 
           {outlistH =  hardenBinds o; 
            nameH = n;
            formalsH = hardenBinds f;
            localsH = generateLocals ll;
            linelistH = ll} in
List.map mdToHmd ast

let literal str = (1, SBuslit str)
let prt name str= (1, SPrint(name, literal str))
let ssast sast = ([], [prt "a" "110"; prt "b" "101"; prt "c" "011"], [])

let check x = ssast (hastToSast (astToHast x))

(*questions:
        * 1) how to close brackets
        * 2) index stuff
        * 3) should indices be done away with before this point?
*)
