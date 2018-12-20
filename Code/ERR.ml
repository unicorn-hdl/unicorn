(* Since semant was unwieldy and hideous enough as is,
 * moved error messages to another file.
 * This makes reading semant easier.
 * It also makes indiscriminately littering error messages
 * with emojis and motivation pretty easy.
 * Note: shorter errors are written in the files that call them*)
open Printer
open Ast

exception InvalidAssignment of string
exception UndeclaredVar of string 
exception TypeMismatch of string
exception InvalidRange of string
exception InvalidCall of string

let getLit exp = match exp with
       Lit(x) -> x
     | x -> p ("missed case: "^ Printer.getIntExpr x); 0

let uvERR name = raise (UndeclaredVar ("Variable \"" ^ name ^ "\" is not defined! Keep trying ❤️"))

let tmERR op l r = raise(TypeMismatch ("You tried performing " ^ opToStr op ^ " on " ^ Printer.getBinExpr "" l ^" and "^Printer.getBinExpr "" r^" but these are of different sizes. But You can fix that!! 😊"))

let tm_assERR rval rtyp x a' b'= raise(TypeMismatch ("You tried assigning " ^ Printer.getBinExpr "" rval^ " of size " ^ string_of_int rtyp ^ " to " ^  x ^ " on the range " ^ string_of_int a' ^ "-" ^ string_of_int b' ^ " but these are of different sizes. Nonetheless, You and Your program are wondrous 😉"))

let irERR x nm outs = raise (InvalidRange ("Uh-oh! You tried to access the "^string_of_int x^"th element of "^nm^" but it only has "^string_of_int (List.length outs)^" outputs!🤷‍♀️"))

let tm_argERR arg argS fm fmS = raise(TypeMismatch ("You tried assigning argument " ^ Printer.getBinExpr ""arg^ " of size "^ string_of_int argS ^ " to formal " ^ fm ^ "<" ^ string_of_int fmS ^ "> but these are of different sizes. They can still get along together, though 🌈"))

let tm_outERR out sz = raise(TypeMismatch ("The output call "^ snd out ^"<"^ string_of_int (getLit (fst out)) ^ "> does not match assignment of size "^ string_of_int sz^"So it goes, I guess ¯\_(ツ)_/¯")) 

let icERR name fms args= raise(InvalidCall ("Call to " ^ name ^ " with " ^ string_of_int (List.length args)^ " arguments but " ^ name ^ " expects " ^ string_of_int (List.length fms) ^ " arguments. Still, Your code's looking great! 🦄"))

