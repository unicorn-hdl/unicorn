(*NOTE much of this topsort code is not our own. 
 * We just wrote interfacing with our netlist for it.
 * All credit goes to RosettaCode.org*)

open Ast
module StringMap = Map.Make(String)

exception Reassign of string

let checkOverride namelist (a,b,c,d) =
    if List.mem a namelist
    then raise (Reassign (a^" has been assigned to twice!"))
    else a::namelist

let topsort (netlist,globs) = 
    let _ = List.fold_left checkOverride [] netlist in
    let getA = List.map (fun (x,_,_) ->x) globs in
    let netlist = ("","","","")::netlist in 
    let foldFn outMap (a,b,c,d) = StringMap.add a (a,b,c,d) outMap in
    let map = List.fold_left foldFn StringMap.empty netlist in
    let lookup x = match x with
       | "0" | "0b" | "1" | "1b" -> ("","","","")
       | _ ->
            if StringMap.mem x map
            then StringMap.find x map
            else if List.mem x getA
            then ("","","","")
            else let _ = print_endline("missed in topsort: "^x) in ("","","","") 
            in
    let foldFn2 lst (a,b,c,d) = 
            if (c=d)
            then  ((a,b,c,d), [lookup c])::lst
            else ((a,b,c,d), [lookup c; lookup d])::lst in
    let dep_libs = List.fold_left foldFn2 [] netlist in

    let _ = print_endline("\n\n") in
    let str4 (a,b,c,d) = ("("^a^", "^b^", "^c^", "^d^")") in
    let str4 (a,b,c,d) = a in
    let printf (a,lst) = 
            let _ = print_string(str4 a^" -> ") in
            let _ = List.iter (fun x->print_string(str4 x^"; ")) lst in
            let _ = print_endline("") in
            () 
    in
    (*let _ = List.iter printf dep_libs in*)


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
      else invalid_arg "un-orderable data"
  | x::xs, _ ->
      let deps = get_deps x in
      let ok = List.for_all (fun dep -> List.mem dep acc) deps in
      if ok
      then aux (x::acc) later xs true
      else aux acc (x::later) xs progress
  in
  let starts, todo = List.partition (fun lib -> get_deps lib = []) libs in
  aux starts [] todo false  in

(res, globs)
