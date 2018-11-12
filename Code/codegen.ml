module L = Llvm
open Sast

let translate x = 
 let context = L.global_context () in 
 let the_module = L.create_module context "UniC" in 
 the_module
