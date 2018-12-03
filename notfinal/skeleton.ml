open Sast

(*what all simplify files should more or less look like. The work is in making the functions in 5-9 do whatever useful thing they need to, instead of just returning what they're fed*)

let doThingToOutlist llist = llist
let doThingToName name = name
let doThingToFormals llist = llist
let doThingToLocals llist = llist
let doThingToLinelist llist = llist

let whatYouDoToEachSmd smd =
	{outlist = doThingToOutlist smd.outlist;
	 name = doThingToName smd.name;
	 formals = doThingToFormals smd.formals;
	 locals = doThingToLocals smd.locals;
	 linelist = doThingToLinelist smd.linelist;
	}

(*smd list-> smd list*)
let thebigfunction smdList =
	List.map whatYouDoToEachSmd smdList
