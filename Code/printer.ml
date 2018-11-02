open Ast
module StringMap = Map.Make(String)

let concat a b = a ^ b
let listToString fn thinglist = List.fold_left concat "" (List.map fn thinglist)

let opToString = function
   And -> "and"
 | Or  -> "or"
 | Nand -> "nand"
 | _ -> "op"

let rec getBinExpr = function
   Buslit(x) -> x
 | BoolId(x) -> x
 | BoolBinop(a,b,c) -> (getBinExpr a) ^ (opToString b) ^ (getBinExpr c)
 | Noexpr -> ""
 | _ -> "im lazy"
let semiColon x = x ^ ";\n"
let toStringBinExprlist explist = listToString semiColon (List.map getBinExpr explist)

let bindFn (a,b,c) =  "" ^ "thing " 
let toStringBindlist blist = listToString bindFn blist

let toStringMod = function
        |Module_decl (outlist, name, formals, linelist) ->
        name ^ "(" ^ (toStringBindlist formals) ^ "){\n" ^ 
        toStringBinExprlist linelist ^ 
        toStringBindlist outlist ^ "\n}\n\n"
let toStringPgm pgm = List.map toStringMod pgm

let _ =
let lexbuf = Lexing.from_channel stdin in
let pgm = Parser.program Scanner.token lexbuf in
let result = listToString (fun x->x) (toStringPgm pgm) in
print_endline (result)
