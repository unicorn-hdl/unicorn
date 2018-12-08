(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
  let context    = L.global_context ();;
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Unic"

  (* Get types from the context *)
  and i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context;;

  (* Return the LLVM type for a MicroC type *)

let translate (netlist, globals) =

  (* Create a map of global variables after creating each *)
(*make global vars*)
  let global_vars : L.llvalue StringMap.t =
    let global_var m n = 
      let init = L.const_int (i1_t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in
    (*TODO: Add real support for globs*)
(*end here*)


  let printf_t : L.lltype = 
          L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
          L.declare_function "printf" printf_t the_module  in

  let printbig_t : L.lltype =
          L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
          L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  
  let function_decls functions: (L.llvalue ) StringMap.t =
    let function_decl m =
      let name = "main"
      and formal_types = 

                Array.init 0 (fun x-> i1_t) 
              (*
	Array.of_list (List.map (fun (t,_) -> i1_t) fdecl.sformals)
*)
      in let ftype = L.function_type (i1_t) formal_types in
      StringMap.add name (L.define_function name ftype the_module) m in
    function_decl StringMap.empty in

(* Fill in the body of the given function *) 
  let build_function_body fdecl =
         
    let the_function = StringMap.find "main"(function_decls netlist) in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
    	let local = L.build_alloca (i1_t) n builder in
        ignore (L.build_store p local builder);
	    StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *) 
        and add_local m (t, n) =
	        let local_var = L.build_alloca (i1_t) n builder in
	        StringMap.add n local_var m in

      let isAssig (l,op,r1,r2) = match op with
             "Print" -> false
           | _ -> true in
      let printlessNL = List.fold_left 
            (fun lst stmt -> if isAssig stmt
                             then stmt :: lst
                             else lst)
            [] netlist in
      let genLocals = List.map (fun (a,b,c,d)->(i1_t,a)) printlessNL in

      let formals = List.fold_left2 add_formal StringMap.empty []
        (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals genLocals 
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> 
                         try StringMap.find n (global_vars)
                         with Not_found -> print_endline("couldn't find "^n); L.const_int i1_t 0
    in

    (* Construct code for an expression; return its value *)
      let getVal = function
        | "0" | "0b" -> L.const_int i1_t 0
        | "1" | "1b" -> L.const_int i1_t 1
        |  x  -> L.build_load (lookup x) x builder
       in

      let rec expr builder (l, op, r1, r2) = match op with
        | "Print" -> 
            L.build_call printf_func [|L.build_global_stringptr (l^": %d\n") "prt" builder; getVal r1|] "printf" builder; builder
        | "And" ->      
            L.build_store (L.build_and (getVal r1) (getVal r2) (l^"'svalue") builder) (lookup l) builder; builder
        | "Or" -> 
            L.build_store (L.build_or (getVal r1) (getVal r2) (l^"'svalue") builder) (lookup l) builder; builder
        | "Xor" ->
            L.build_store (L.build_xor (getVal r1) (getVal r2) (l^"'svalue") builder) (lookup l) builder; builder
        | "Ident" -> 
            L.build_store (getVal r1) (lookup l) builder; builder
        | "Not" ->
            L.build_store (L.build_not (getVal r1) (l^"'svalue") builder) (lookup l) builder; builder
        | a -> 
            print_endline("ERROR: there is an unrecognized op in expr in codegen"); builder

      in
    
    let builder = List.fold_left expr builder netlist in
    
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)


    let add_terminal builder instr =
      ignore(print_endline ("yoyoyo"));
      match L.block_terminator (L.insertion_block builder) with
	    Some _ -> ()
      | None -> ignore (instr builder) in
	
    add_terminal builder 
       (L.build_ret (L.const_int (i1_t) 0))
  in

  build_function_body netlist;
  the_module
