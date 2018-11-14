module L = Llvm
(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast | LLVM_IR | Compile

let () =
  let printsmt = print_endline("thisotherthing!") in
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./microc.native [-a|-s|-l|-c] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = Lexing.from_channel !channel in
  let printsmt = print_endline("thisthing!") in 
(*fake ast 
  let ast = Parser.program Scanner.token lexbuf in  
*)
let ast = [([(Ast.Lit(1), "input1")], "moduleName", [(Ast.Lit(1), "output1")], [])] in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | _ -> let ssast = Semant.check ast in
      match !action with
        Ast     -> ()
      | Sast    -> print_string (Sast.string_of_sprogram ssast)
      | LLVM_IR -> print_string (L.string_of_llmodule (Codegen.translate ssast))
      | Compile -> let m = Codegen.translate ssast in
	  Llvm_analysis.assert_valid_module m;
	  print_string (L.string_of_llmodule m)
