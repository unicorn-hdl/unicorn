(*NOTE much of this topsort code is not our own. 
 * We just wrote interfacing with our netlist for it.
 * All credit goes to RosettaCode.org*)

open Ast
module StringMap = Map.Make(String)


let topsort netlist = 
    let getStr = function
        | Buslit(x) -> x
        | BoolId(x) -> x 
        | Index(Buslit(x),_) -> x
        | Index(BoolId(x),_) -> x
        | x -> print_endline("missed getStr case: "^ Printer.getBinExpr x); "" in
    let foldFn outMap expr = match expr with
        | Assign(_,x,_,_) -> StringMap.add (getStr x) expr outMap
        | Print(x,_) -> StringMap.add x expr outMap in
    let map = List.fold_left foldFn StringMap.empty netlist in
    let lookup = function
        | Buslit(x) -> Buslit(x)
        | BoolId(x) -> StringMap.find x map in
    let _ = print_endline ("\nA map!") in
    let _ = StringMap.iter (fun k v -> print_string (k^", ") ) map in
    let _ = print_endline ("end mapprint") in
    let dependencies expr = 
        let rval =
            match expr with
            | Assign(_,_,x,_) -> x
            | Print(_,x) -> x in
        match rval with
        | Buslit(x) -> []
        | BoolId(x) -> print_endline("1: "^x); [StringMap.find x map]
        | BoolBinop(l,op,r) -> print_endline("2"); [lookup l ; lookup r]
        | Unop(op,ex) -> print_endline("3");[lookup ex]
        | Assign(_,_,_,_) -> print_endline("a thing that should never happen did");[]
        | Index(ex,_) -> print_endline("4: "^Printer.getBinExpr ex);[lookup ex]
        | Print(_,_) -> print_endline("a thing that should never happen did");[]
        | Call(_,_) -> print_endline("a thing that should never happen did");[] 
        | For(_,_,_) -> print_endline("a thing that should never happen did");[]
        | ModExpr(_,_,_) -> print_endline("a thing that should never happen did");[]
        | Noexpr -> [] in

    let foldFn2 outList expr = (expr, (dependencies expr))::outList in
    let dep_libs = List.fold_left foldFn2 [] netlist in

(*Code here and beyond is not ours*)
let dep_libs =
  let f (lib, deps) =  (* remove self dependency *)
    (lib,
     List.filter (fun d -> d <> lib) deps) in
  List.map f dep_libs in
 
let rev_unique =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) [] in
 
let libs =  (* list items, each being unique *)
  rev_unique (List.flatten(List.map (fun (lib, deps) -> lib::deps) dep_libs)) in
 
let get_deps lib =
  try (List.assoc lib dep_libs)
  with Not_found -> [] in
 
let res =
  let rec aux acc later todo progress =
  match todo, later with
  | [], [] -> (List.rev acc)
  | [], _ ->
      if progress
      then aux acc [] later false
      else let _ = print_endline("un-orderable data5") in netlist
  | x::xs, _ ->
      let deps = get_deps x in
      let ok = List.for_all (fun dep -> List.mem dep acc) deps in
      if ok
      then aux (x::acc) later xs true
      else aux acc (x::later) xs progress
  in
  let starts, todo = List.partition (fun lib -> get_deps lib = []) libs in
  aux starts [] todo false  in

res
