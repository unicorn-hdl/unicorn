(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

This is heavily based off of MicroC's code generation
*)

module L = Llvm
module StringMap = Map.Make(String)

exception UndeclaredVar of string 

let context = L.global_context ()
  
(* Create the LLVM compilation module into which
   we will generate code *)
let the_module = L.create_module context "Unic"

(* Get types from the context *)
let i32_t = L.i32_type context
let i8_t = L.i8_type context
let i1_t = L.i1_type context;;

(* translate : ((a,b,c,d),(d,e,f)) -> Llvm.module *)
let translate (netlist, globals) =

  (*make global vars*)
  let global_vars : L.llvalue StringMap.t =
     let global_var m (n,_,i) = 
        let init = L.const_int i1_t (int_of_string i) in
        StringMap.add n (L.define_global n init the_module) m in
     List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
          L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
          L.declare_function "printf" printf_t the_module  in

  (* Define the call to tick, so we can fill in its body*) 
  let function_decls : (L.llvalue ) StringMap.t =
    let function_decl m =
      let name = "tick"
      and formal_types = 
                Array.init 0 (fun _-> i1_t) in
      let ftype = L.function_type (i1_t) formal_types in
      StringMap.add name (L.define_function name ftype the_module) m in
    function_decl StringMap.empty in

  (* Dump our netlist into tick *) 
  let build_function_body =
         
    let tick_fn = StringMap.find "tick" function_decls in
    let builder = L.builder_at_end context (L.entry_block tick_fn) in

    (* Construct the tick's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (_, n) p = 
        L.set_value_name n p;
    	let local = L.build_alloca (i1_t) n builder in
        ignore (L.build_store p local builder);
	    StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *) 
        and add_local m (_, n) =
	        let local_var = L.build_alloca (i1_t) n builder in
            if StringMap.mem n global_vars
            then m
            else StringMap.add n local_var m in

      let isAssig (_,op,_,_) = match op with
             "Print" -> false
           | _ -> true in
      let printlessNL = List.fold_left 
            (fun lst stmt -> if isAssig stmt
                             then stmt :: lst
                             else lst)
            [] netlist in
      let genLocals = List.map (fun (a,_,_,_)->(i1_t,a)) printlessNL in

      let formals = List.fold_left2 add_formal StringMap.empty []
        (Array.to_list (L.params tick_fn)) in
      List.fold_left add_local formals genLocals 
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> 
                         try StringMap.find n (global_vars)
                         with Not_found -> raise (UndeclaredVar ("ERROR: The value of "^n^" was never defined!"))
    in

    (* Construct code for an expression; return its value *)
      let getVal = function
        | "0" | "0b" -> L.const_int i1_t 0
        | "1" | "1b" -> L.const_int i1_t 1
        |  x  -> L.build_load (lookup x) x builder
       in

      let expr builder (l, op, r1, r2) = match op with
        | "Print" -> 
            let _ = L.build_call printf_func [|L.build_global_stringptr (l^": %d\n") "prt" builder; getVal r1|] "printf" builder in builder
        | "And" ->      
            let _ = L.build_store (L.build_and (getVal r1) (getVal r2) (l^"'svalue") builder) (lookup l) builder in builder
        | "Or" -> 
            let _ = L.build_store (L.build_or (getVal r1) (getVal r2) (l^"'svalue") builder) (lookup l) builder in builder
        | "Xor" ->
            let _ = L.build_store (L.build_xor (getVal r1) (getVal r2) (l^"'svalue") builder) (lookup l) builder in builder
        | "Ident" -> 
            let _ = L.build_store (getVal r1) (lookup l) builder in builder
        | "Not" ->
            let _ = L.build_store (L.build_not (getVal r1) (l^"'svalue") builder) (lookup l) builder in builder
        | _ -> builder
      in
    
    let builder = List.fold_left expr builder netlist in
    
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	    Some _ -> ()
      | None -> ignore (instr builder) in
	
    add_terminal builder 
       (L.build_ret (L.const_int (i1_t) 0))
  in

  build_function_body;
  the_module
