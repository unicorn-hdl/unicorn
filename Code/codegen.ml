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
                   with Not_found -> StringMap.find n (global_vars)
    in

    (* Construct code for an expression; return its value *)

      let rec expr builder (l, op, r1, r2) = match op with
        | "Print" -> L.build_call printf_func [|L.build_global_stringptr (l^": %d\n") "prt" builder; lookup r1|] "printf" builder; builder
        | "Print" -> L.build_call printf_func [|int_format_str; L.const_int i1_t 0|] "printf" builder; builder
        | a -> L.const_int i1_t 0; builder

        (*
    | SBuslit b -> ignore(print_endline ("lit " ^ b));L.const_int i32_t (int_of_string ("0b" ^ b))
    | SPrint (str, e) -> ignore(print_string ("print " ^ str));
    (* The bit for dealing with strings is fussy bc no built-in string_type*)
    (*
          L.build_call printf_func [|L.const_string context (str ^ ": ")|] "printf" builder;
     *)
      L.build_call printf_func [|int_format_str; expr e|] "printf" builder
    | SSAssign (true, s, e, _) -> 
        ignore(print_endline("we're doing an assign!"));
        let e' = expr e in
        ignore(L.build_store e' (lookup s) builder); e'
    | _ -> ignore (print_endline "nomatch! "); L.const_int i32_t 0
    *)

      in

    (*
    let rec expr builder ((_, e) : sexpr) = match e with
	SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with 
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mult    -> L.build_fmul
	  | A.Div     -> L.build_fdiv 
	  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Neq     -> L.build_fcmp L.Fcmp.One
	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	  | A.And | A.Or ->
	      raise (Failure "internal error: semant should have rejected and/or on float")
	  ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
          let e' = expr builder e in
	  (match op with
	    A.Neg when t = A.Float -> L.build_fneg 
	  | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | SCall ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | SCall ("printf", [e]) -> 
	  L.build_call printf_func [| float_format_str ; (expr builder e) |]
	    "printf" builder
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f (function_decls functions) in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
    in
    *)
    
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
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    (*
    let rec stmt builder = function
      | SExpr e -> ignore(expr builder e); builder 
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
      (*
                              A.Void -> L.build_ret_void builder 
                              *)
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in
    *)

    (* Build the code for each statement in the function *)
    
    (*
    let builder = stmt builder (SBlock fdecl.sbody) in
    *)

    (* Add a return if the last block falls off the end *)
    add_terminal builder 
    (*
    (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
    *)
       (L.build_ret (L.const_int (i1_t) 0))
  in

  build_function_body netlist;
  the_module
