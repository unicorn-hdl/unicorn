module L = Llvm
(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast | LLVM_IR | Compile

let () =
  let action = ignore (print_string "1"); ref Compile in
  let set_action a () = action := a in
  let speclist = ignore (print_string "2"); [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = ignore (print_string "3"); "usage: ./microc.native [-a|-s|-l|-c] [file.mc]" in
  let channel = ignore (print_string "4");ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = ignore (print_string "5"); Lexing.from_channel !channel in
  let ast = ignore (print_string "6"); Parser.program Scanner.token lexbuf in  

  match !action with
    Ast -> ignore (print_string "7"); print_string (Ast.string_of_program ast)
  | _ -> ignore (print_string "8"); let ssast = Semant.check ast in
      match !action with
        Ast     -> ignore (print_string "9"); ()
      | Sast    -> ignore (print_string "10 "); print_string (Sast.string_of_sprogram ssast)
      | LLVM_IR -> ignore (print_string "11 "); print_string (L.string_of_llmodule (Codegen.translate ssast))
      | Compile -> ignore (print_string "12 "); let m = Codegen.translate ssast in
	  Llvm_analysis.assert_valid_module m;
	  print_string (L.string_of_llmodule m)
