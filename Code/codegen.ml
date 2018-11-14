module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (inlist,  binlist, outlist) =
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
  let the_main_function = L.define_function "mainfn" mainfn_typ the_module in
  let builder = L.builder_at_end context (L.entry_block the_main_function) in

  let printf_t: L.lltype =
     L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func: L.llvalue = 
     L.declare_function "printf" printf_t the_module in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

  let rec expr ( (_,e): sbinExpr) = match e with
    | SBuslit b -> ignore(print_endline ("lit " ^ b));L.const_int i32_t (int_of_string ("0b" ^ b))
    | SPrint (str, e) -> ignore(print_string ("print " ^ str)); 
    (* The bit for dealing with strings is fussy bc no built-in string_type
     *
          L.build_call printf_func 
                       [|L.const_float_of_string (str ^ ": ")|] 
                       "printf" builder;
     *)
          L.build_call printf_func 
                       [|int_format_str; expr e|] 
                       "printf" builder
    | _ -> ignore (print_endline "nomatch! "); L.const_int i32_t 0
  in  

  let builder = L.block_terminator (L.insertion_block builder) in
  ignore(List.map expr binlist)
);
 the_module
