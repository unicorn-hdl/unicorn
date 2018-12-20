(*Nothing much to say. Grabs, an AST, 
 * binExpr, netlist, or netlist2 and
 * spits out stuff onto the screen
 *
 * It's all pretty self-explanatory
 *
 * I guess what I'm tying to say is that maybe
 * since Printer makes pretty things it doesn't
 * need to be pretty—it has a transitive kind of beauty
 * (through a layer of indrection, one might say) *)

open Ast
module StringMap = Map.Make(String)

let p x = print_endline x
let sOfI x = string_of_int x

let concat a b = a ^ b
let listToString fn thinglist = List.fold_left concat "" (List.map fn thinglist)

let rec getIntExpr = function
 | IntBinop(lval, Add, rval) -> getIntExpr lval ^ "+" ^ getIntExpr rval
 | IntBinop(lval, Sub, rval) -> getIntExpr lval ^ "-" ^ getIntExpr rval
 | Lit(x) -> string_of_int x
 | IntId(x) -> x

let index = function | Range (a,b) ->  "[" ^ getIntExpr a ^ ":" ^ getIntExpr b ^ "]"

let bindFn (b,c) = c ^ "<"^getIntExpr b^"> "

let toStringBindlist blist = listToString bindFn blist

(*returns string for binExpr*)
let rec getBinExpr tabs = function
   Buslit(x) -> x
 | BoolId(x) -> x
 | BoolBinop(a,b,c) -> (getBinExpr tabs a) ^ " " ^ (opToStr (B(b))) ^ " " ^ (getBinExpr tabs c)
 | Unop(Not,a) -> "~" ^ getBinExpr tabs a
 | Noexpr -> "noexpr"
 | Index(expr, ind) -> getBinExpr tabs expr ^ index ind
 | Assign(isReg, lval, rval, initval) -> if isReg
        then getBinExpr tabs lval ^ ":= " ^ getBinExpr tabs rval ^ " init " ^ initval
        else getBinExpr tabs lval ^ "= " ^ getBinExpr tabs rval
 | Call(id, arglist) -> id ^ "(" ^ listToString (fun x-> getBinExpr tabs x ^ ",") arglist ^ ")"
 | Print(id, x) -> "print " ^ id ^ ":  " ^ getBinExpr tabs x 
 | For(var, Range(a,b), lines) -> "for(" ^ var ^ " from "^ (getIntExpr a)^ " to "^ (getIntExpr b)^ "){\n" ^ toStringBinExprlist (tabs^"   ") lines ^ tabs^"}"
 | ModExpr(MD(_,nm,_,lns), args) -> 
        nm ^ "(" ^ (List.fold_left (fun inStr x-> inStr^getBinExpr tabs x^" ") "" args)^ "){\n" ^ 
        toStringBinExprlist (tabs^"   ") lns^ tabs^"}"
 | _ -> "?"

 and makeline tabs x = tabs^x^";\n"
 and toStringBinExprlist tabs explist = listToString (makeline tabs) (List.map (getBinExpr tabs) explist)

 and toStringMod (MD (outlist, name, formals, linelist)) =
        name ^ "(" ^ (toStringBindlist formals) ^ "){\n" ^ 
        toStringBinExprlist "" linelist ^ 
        "   out: " ^ toStringBindlist outlist ^ "\n}\n\n"

let toStringPgm pgm = List.map toStringMod pgm

let printAst pgm = print_endline ("\n\n~~PRINTING AST~~\n");
                   print_endline (listToString (fun x->x) (toStringPgm pgm))

let printMast pgm = print_endline ("\n\n~~PRINTING MAST~~\n");
                   print_endline (getBinExpr "" pgm)

let printNet pgm = print_endline ("\n\n~~PRINTING NAST~~\n");
                    List.iter (fun x->print_endline (getBinExpr "" x^";")) pgm

let printNet2 pgm = print_endline ("\n\n~~PRINTING NAST2~~\n\n");
                    List.iter (fun (a,b,c,d)->print_endline ("{"^ 
                    a^ ", "^ b^ ", "^ c^ ", "^ d^"}")) 
                    (fst pgm)
