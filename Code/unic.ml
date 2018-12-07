module L = Llvm
module C = Codegen
module P = Printer
module E = Elaborate
module F = Noloop2
module SL = Simplelines
module I = Indexing
(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast | Mast | Netlist | Forloops | SimpleLines | Index | Netlist2 | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a",  Arg.Unit (set_action Ast), "Print the AST");
    ("-m",  Arg.Unit (set_action Mast), "Print the modfilled AST");
    ("-s",  Arg.Unit (set_action Sast), "Print the SAST");
    ("-n",  Arg.Unit (set_action Netlist), "Print Netlist");
    ("-f",  Arg.Unit (set_action Forloops), "Print Netlist with collapsed for loops");
    ("-sl", Arg.Unit (set_action SimpleLines), "Print Netlist with Simplified Lines");
    ("-i",  Arg.Unit (set_action Index), "Print Netlist with collapsed inidices");
    ("-n2", Arg.Unit (set_action Netlist2), "Print MoreSimplified Netlist");
    ("-l",  Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c",  Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./microc.native [-a|-s|-l|-c] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in  
  let hast = Harden2.harden (Modfill.fill ast) in

  match !action with
      Ast -> P.printAst ast
    | _ -> let ssast = Semant.check hast in
         match !action with
              Ast     -> ()
            | Mast    -> P.printMast hast
            | _       -> let netlist = E.collapse hast in 
                match !action with
                      Ast -> ()
                    | Mast-> ()
                    | Netlist -> P.printNet netlist
                    | Forloops -> P.printNet (F.unloop netlist)
                    | SimpleLines -> P.printNet (SL.simplify (F.unloop netlist))
                    | Index -> P.printNet (I.index (SL.simplify (F.unloop netlist)))
                    | _ -> let netlist2 = E.collapse2 (I.index (SL.simplify (F.unloop netlist))) in
                        match !action with
                              Ast -> ()
                            | Mast -> ()
                            | Netlist -> ()
                            | Forloops -> ()
                            | SimpleLines -> ()
                            | Netlist2 -> P.printNet2 netlist2
                            | Sast    -> print_string ("")
                            | LLVM_IR -> print_string (L.string_of_llmodule (C.translate netlist2))
                            | Compile -> let m = C.translate netlist2 in
	                                Llvm_analysis.assert_valid_module m;
	                                print_string (L.string_of_llmodule m)
