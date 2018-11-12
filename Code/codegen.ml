module L = Llvm
module A = Ast
open Semant

module StringMap = Map.Make(String)

(* translate: Sast.program -> Llvm.module *)
let translate mds = 
 let context = L.global_context () in (*just some LLVM init'ing, I think*)

 (*Create the module. This is what the translate returns*)
 let the_module = L.create_module context "UniC" in  

 (*get LLVM types*)
 let i32_t      = L.i32_type    context
 and i_8        = L.i8_type     context
 and i1_t       = L.i1_type     context
 and void_t     = L.void_type   context in

 (*And pass the above types to our Ast types*)
 (*I am assuming that there are longer any intExprs, but rather only busses at this point. If there are intExprs with ints to be eval'd, write let int -> if x >= 0 then i32_t else 132_t*)
 let getLtyp = function
      x -> i32_t in 
 (*Note: busSize must be limited to 32 bits. If we want bigger, need to change line above this*)
 
(*Some useful fns*)
let mapfn (t,_) = getLtyp t in

 (*Define modules*)
 let modz : (L.llvalue * hardSmd) StringMap.t = 
    let md mmmm mdec = 
        let name = mdec.name
        and formaltypes =
        Array.of_list (List.map mapfn mdec.formals) in
        let ftype = L.function_type (getLtyp mdec.outlistH) formaltypes in

 StringMap.add name (L.define_function name ftype the_module, mdec) mmmm in
    List.fold_left md StringMap.empty mds in


 (*Fill a module's (modz) body*)
 let buildModBod mdec =
    let (modulz, _) = StringMap.find mdec.sname modz in
    let builder = L.builder_at_end contect (L.entry_block modulz) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" build in 

    (*Make all the module's internal busses. Allocate on stack, init val, put val in "locals" map*)
    let vars =
        let addFormal m (t,n) p = L.set_value_name n p;
    let local = L.build_alloca (getLtyp t) n builder in
    ignore (L.build_store p local builder);
    StringMap.add n local m

    (*Allocate space for locals and put in map*)
    and addLocal m (t,n) =
        let vars = L.build_alloca (getLtyp t) n builder in
        StringMap.add n var m in

            let formals = List.fold_left2 addFormal Stringmap.empty mdec.formals (Array.to_list (L.params modulz)) in
            List.fold_left addLocal formals mdec.locals in

    (*Return vars' values*)
    let lookup n = StringMap.find n vars in

    (*Code for expressions. Return their values*)
    let rec expr build ((_,e): sexpr) = match e with
      SBustlit b -> L.const_int i32_t b 
    | SCall (f, args) -> let (moddef, moddec) = StringMap.find f modz in
    
    let llargs = List.rev (List.map (expr builder) (List.rev args)) in
    let result = (match mdec.typ with 
                       A.void -> ""
                     | _ -> f ^ "_result") in
            L.build_call fdef (Array.of_list llargs) result builder in

    (*Make terminators 🤖☠️  *)
    let addTerminal build instr =
        match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder) in

    (*Code for each line. Return the builder for next line*)
    let rec stmt builder = function
          SLine s1 -> List.fold_left stmt builder sl
        | SBinExpr e -> ignore (expr builder e); builder

    (*Build code for stmts*)
    let builder = stmt builder (SLine mdec.body) in 

    List.iter buildModBod mds;

 the_module
