module L = Llvm
(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast | Mast | Netlist | Netlist2 | LLVM_IR | Compile

let () =
  let action = (* ignore (print_string "1") ;*) ref Compile in
  let set_action a () = action := a in
  let speclist = (* ignore (print_string "2"); *) [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-m", Arg.Unit (set_action Mast), "Print the modfilled AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-n", Arg.Unit (set_action Netlist), "Print Netlist");
    ("-n2", Arg.Unit (set_action Netlist2), "Print MoreSimplified Netlist");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = (* ignore (print_string "3"); *) "usage: ./microc.native [-a|-s|-l|-c] [file.mc]" in
  let channel = (* ignore (print_string "4"); *) ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = (* ignore (print_string "5"); *) Lexing.from_channel !channel in
  let ast = (* ignore (print_string "6"); *) Parser.program Scanner.token lexbuf in  
  let hast = Harden2.harden (Modfill.fill ast) in

  match !action with
    Ast -> (* ignore (print_string "7"); *) Printer.printAst ast
  | _ -> (* ignore (print_string "8"); *) let ssast = Semant.check hast in
      match !action with
        Ast     -> (* ignore (print_string "9"); *) ()
      | Mast    -> Printer.printMast hast
      | Netlist -> 
            let netlist = Elaborate.collapse hast in
            Printer.printNet netlist
      | Netlist2 -> 
            let netlist2 = Elaborate.collapse2 (Elaborate.collapse hast) in
            Printer.printNet2 netlist2
      | Sast    ->  (* ignore (print_string "10 "); *) print_string ("heyo")
      | LLVM_IR -> ()
      | Compile -> ()
      (*
      | LLVM_IR -> (* ignore (print_string "11 "); *) print_string (L.string_of_llmodule (Codegen.translate ssast))
      | Compile -> (* ignore (print_string "12 "); *) let m = Codegen.translate ssast in
	  Llvm_analysis.assert_valid_module m;
	  print_string (L.string_of_llmodule m)
*)
