let visited = [];;
let orig_netlist = [("e", "b"); ("j", "f")];;

findstarts [] [] orig_netlist;;

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1;;
let starts, ends = findstarts [] [] orig_netlist;;

 let rec findstarts starts ends = function
         [] -> (starts, ends)
          | (s, e) :: tail ->
                          if List.mem s starts then
if List.mem e ends then
findstarts starts ends tail
else findstarts starts (e::ends) tail
else if List.mem e ends then
findstarts (s::starts) ends tail
else findstarts (s::starts) (e::ends) tail;;

let rec successors n = function
         [] -> visited
         |(s, t) :: orig_netlist ->
                       if s = n then
                                 t::successors n orig_netlist
                         else successors n orig_netlist;;

let rec topsort orig_netlist visited = function
         [] -> visited
         |n::nodes ->
let visited' = if List.mem n visited then visited
         else n::topsort orig_netlist visited(successors n orig_netlist)
in topsort orig_netlist visited' nodes;;


topsort orig_netlist visited ["e"];; (*need to give starting point for netlist like ["e"]*)

(*let myPrint (x) = print_endline x;;
List.iter myPrint visited;;*)
