open Sast

(*what all simplify files should more or less look like. The work is in making the functions in 5-9 do whatever useful thing they need to, instead of just returning what they're fed*)

let dothings = function
        |SFor(a,b,c) -> replaceforloop a b c
        | a -> a

let doThingToOutlist llist = llist
let doThingToName name = name
let doThingToFormals llist = llist
let doThingToLocals llist = llist
let doThingToLinelist llist = List.map dothings llist;
        
let whatYouDoToEachSmd smd =
	{outlist = doThingToOutlist smd.outlist;
	 name = doThingToName smd.name;
	 formals = doThingToFormals smd.formals;
	 locals = doThingToLocals smd.locals;
	 linelist = doThingToLinelist smd.linelist;
	}

let noloops smdList =
	List.map callOnSmd smdList
