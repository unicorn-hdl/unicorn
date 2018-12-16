open Ast
module StringMap = Map.Make(String)

let p x = print_endline x
let sOfI x = string_of_int x

let concat a b = a ^ b
let listToString fn thinglist = List.fold_left concat "" (List.map fn thinglist)

let opToString = function
   And -> "and"
 | Or  -> "or"
 | Nand -> "nand"
 | _ -> "op"
(*someone can finish this bit*)

let rec getIntExpr = function
 | IntBinop(lval, Add, rval) -> getIntExpr lval ^ "+" ^ getIntExpr rval
 | IntBinop(lval, Sub, rval) -> getIntExpr lval ^ "-" ^ getIntExpr rval
 | Lit(x) -> string_of_int x
 | IntId(x) -> x

let index = function | Range (a,b) ->  "[" ^ getIntExpr a ^ ":" ^ getIntExpr b ^ "]"

let bindFn (b,c) = match b with 
   Lit(x) -> c ^ "<" ^ string_of_int x ^ "> " 
 | _ -> c ^ "<"^getIntExpr b^"> "
let toStringBindlist blist = listToString bindFn blist

let rec getBinExpr = function
   Buslit(x) -> x
 | BoolId(x) -> x
 | BoolBinop(a,b,c) -> (getBinExpr a) ^ " " ^ (opToString b) ^ " " ^ (getBinExpr c)
 | Unop(Not,a) -> "~" ^ getBinExpr a
 | Noexpr -> "noexpr"
 | Index(expr, ind) -> getBinExpr expr ^ index ind
 | Assign(isReg, lval, rval, initval) -> if isReg
        then getBinExpr lval ^ ":= " ^ getBinExpr rval ^ " init " ^ initval
        else getBinExpr lval ^ "= " ^ getBinExpr rval
 | Call(id, arglist) -> id ^ "(" ^ listToString (fun x-> getBinExpr x ^ ",") arglist ^ ")"
 | Print(id, x) -> "print " ^ id ^ ":  " ^ getBinExpr x ^ ";"
 | For(var, Range(a,b), lines) -> "for(" ^ var ^ " from "^ (getIntExpr a)^ " to "^ (getIntExpr b)^ "){\n" ^ toStringBinExprlist lines
 | ModExpr(modz, args) -> 
                 "\n\tin: " ^ listToString (fun x-> getBinExpr x ^ ",") args ^
                 "\n\t" ^ toStringMod modz
 | _ -> "?"

 and makeline x = "\t" ^ x ^ ";\n"
 and toStringBinExprlist explist = listToString makeline (List.map getBinExpr explist)

 and toStringMod = function
        |MD (outlist, name, formals, linelist) ->
        name ^ "(" ^ (toStringBindlist formals) ^ "){\n" ^ 
        toStringBinExprlist linelist ^ 
        "\tout: " ^ toStringBindlist outlist ^ "\n}\n\n"
let toStringPgm pgm = List.map toStringMod pgm

(*
let _ =
let lexbuf = Lexing.from_channel stdin in
let pgm = Parser.program Scanner.token lexbuf in
let result = listToString (fun x->x) (toStringPgm pgm) in
print_endline (result)
*)

let printAst pgm = print_endline ("\n\n~~PRINTING AST~~\n");
                   print_endline (listToString (fun x->x) (toStringPgm pgm))

let printMast pgm = print_endline ("\n\n~~PRINTING MAST~~\n");
                   print_endline (getBinExpr pgm)

let printNet pgm = print_endline ("\n\n~~PRINTING NAST~~\n");
                    List.iter (fun x->print_endline (getBinExpr x^";")) pgm

let printNet2 pgm = print_endline ("\n\n~~PRINTING NAST2~~\n\n");
                    List.iter (fun (a,b,c,d)->print_endline ("{"^ 
                    a^ ", "^ b^ ", "^ c^ ", "^ d^"}")) 
                    (fst pgm)