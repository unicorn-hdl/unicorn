let visited = [];;
let orig_netlist = [("e", "b"); ("j", "f")];;
let sorted_netlist = [];;

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


topsort orig_netlist visited;;

let myPrint (x) = print_endline x;;
List.iter myPrint visited;;
