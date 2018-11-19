open Sast

(*what all simplify files should more or less look like. The work is in making the functions in 2-6 do whatever useful thing they need to, instead of just returning what they're fed*)

let doThingToOutlist list = list
let doThingToName name = name
let doThingToFormals list = list
let doThingToLocals list = list
let doThingToLinelist list = list

let whatYouDoToEachSmd smd =
	{outlist = doThingToOutlist smd.outlist;
	 name = doThingToName smd.name;
	 formals = doThingToFormals smd.formals;
	 locals = doThingToLocals smd.locals;
	 linelist = doThingToLinelist smd.linelist;
	}

let thebigfunction smdList =
	List.map whatYouDoToEachSmd smdList
