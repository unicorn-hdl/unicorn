module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (inlist, locals, binlist, outlist) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "UniC" in

(
  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and void_t     = L.void_type   context in
  
  let mainfn_typ = L.function_type void_t [|i32_t|] in 
  let the_main_function = L.define_function "main" mainfn_typ the_module in
  let builder = L.builder_at_end context (L.entry_block the_main_function) in

  let printf_t: L.lltype =
     L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func: L.llvalue = 
     L.declare_function "printf" printf_t the_module in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in


    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	  let local = L.build_alloca (i32_t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (i32_t) n builder
	in StringMap.add n local_var m 
      in

      let formals = ignore(print_endline ("this thing: ") ); List.fold_left2 add_formal StringMap.empty inlist 
          (Array.to_list (L.params the_main_function)) in
      List.fold_left add_local formals locals 
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)

      let printline a b = print_endline ("var called: " ^ a) in
      let lookup n = ignore(print_endline("stringmap is empty: " ^ string_of_bool (StringMap.is_empty local_vars))); ignore(StringMap.iter printline local_vars);try StringMap.find n local_vars
    with Not_found -> ignore(print_endline("variable " ^ n ^ "does not exist!")); L.const_int i1_t 1  
    in
  
  let rec expr ( (_,e): sbinExpr) = match e with
    | SBuslit b -> ignore(print_endline ("lit " ^ b));L.const_int i32_t (int_of_string ("0b" ^ b))
    | SPrint (str, e) -> ignore(print_string ("print " ^ str)); 
    (* The bit for dealing with strings is fussy bc no built-in string_type*)
    (*
          L.build_call printf_func [|L.const_string context (str ^ ": ")|] "printf" builder;
     *)
      L.build_call printf_func [|int_format_str; expr e|] "printf" builder
    | SSAssign (true, s, e, _) -> ignore(print_endline("we're doing an assign!"));let e' = expr e in
        ignore(L.build_store e' (lookup s) builder); e'
    | _ -> ignore (print_endline "nomatch! "); L.const_int i32_t 0
  in  

  ignore(List.map expr binlist);
  ignore(L.build_ret_void (L.builder_at_end context (L.entry_block the_main_function)));
  ignore(L.block_terminator (L.insertion_block builder));
);
 the_module
