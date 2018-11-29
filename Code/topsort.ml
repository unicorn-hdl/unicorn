open Ast

(* 

  NETLIST (Stuff that happens before)
  - Receive list of pattern-matched tupples of type binExpr or UnExpr

1. take netlist and ensure the tuple form (md * [listOfExprs]) to tie
the module to the netlist expression being sorted

2. Create a new list of tuples of kind (md * [listOfMds])

3. iterate through listOfExprs, if md found, add to listOfMds of that md

4. Using list of tuples of (md * listOfMds), extract the operator

5. Apply the following in-degrees to the list objects based on the operator:
---> (in, gates (order of modules? arbitrary?), outs)

6. Output of topsort should go into nomodules

*)



(*open netlist*)
open Sast

let orig_netlist = [("e", "b", "c", "OR"); ("j", "f", "k", "NOT")]
in
let visited = []
in
(* successor function gives list of every direct successor to a node. Argument n is the node which the function will return the successor of*)
let rec topsort orig_netlist visited = function
         [] -> visited
         |n::nodes ->
let rec successors n = function
         [] -> visited
         |(s, t) :: orig_netlist ->
                       if s = n then
                                 t::successors n orig_netlist
                         else successors n orig_netlist in         
let visited' = if List.mem n visited then visited
         else n::topsort orig_netlist visited(successors n orig_netlist)
in topsort orig_netlist visited' nodes;;





(* Gael's original topsort code*)


(*
let unordered_modules = [.md * A.md::tail]

let ordered_modules =
  let f (modules, indegree) = 
    (ordered_modules,
     List.filter (fun op -> op <> modList) op) in
  List.map op modList
 
let modList =  
  uniques (List.flatten(List.map (fun (op, indegree) -> modList::tail) indegree))
 
let get_indegree lib =
  try (List.assoc modList indegree)
  (* incomplete *)

(* pretty straightforward algorith goes here *)
 
let () =
  print_string "result: \n ";
  print_endline (String.concat ", " res);
;;



*)
