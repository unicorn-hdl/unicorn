module L = Llvm
module C = Codegen
module P = Printer
module E = Elaborate
module F = Noloop2
module SL = Simplelines
module T0 = Topsort5
module T = Topsort4
module I = Indexing
(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast | Mast | Hast | Netlist | Forloops | SimpleLines | SimpleLinesTop | Index | Netlist2 | Topsort | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a",  Arg.Unit (set_action Ast), "Print the AST");
    ("-m",  Arg.Unit (set_action Mast), "Print the modfilled AST");
    ("-h",  Arg.Unit (set_action Hast), "Print the hardened AST");
    ("-s",  Arg.Unit (set_action Sast), "Print the SAST");
    ("-n",  Arg.Unit (set_action Netlist), "Print Netlist");
    ("-f",  Arg.Unit (set_action Forloops), "Print Netlist with collapsed for loops");
    ("-sl", Arg.Unit (set_action SimpleLines), "Print Netlist with Simplified Lines");
    ("-st", Arg.Unit (set_action SimpleLinesTop), "Print Topsorted Netlist with Simplified Lines");
    ("-i",  Arg.Unit (set_action Index), "Print Netlist with collapsed inidices");
    ("-n2", Arg.Unit (set_action Netlist2), "Print MoreSimplified Netlist");
    ("-t",  Arg.Unit (set_action Topsort), "Print Topsorted Netlist");
    ("-l",  Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c",  Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./microc.native [-a|-s|-l|-c] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in  
  let hast = Harden2.harden (F.unloop (Modfill.fill ast)) in

  match !action with
      Ast  -> P.printAst ast
    | Mast -> P.printMast (Modfill.fill ast)
    | Forloops -> P.printMast (F.unloop (Modfill.fill ast))
    | Hast -> P.printMast hast
    | Sast -> Semant.check hast; ()
    | _    -> 
        let _ = Semant.check hast in
        let netlist = E.collapse hast in 
        match !action with
            Ast -> ()
          | Mast-> ()
          | Sast-> ()
          | Forloops -> () 
          | Netlist -> P.printNet netlist
          | SimpleLines -> P.printNet (SL.simplify (netlist))
          (*
          | SimpleLinesTop -> P.printNet (T0.topsort (SL.simplify (F.unloop netlist)))
          | Index -> P.printNet (I.index (T0.topsort (SL.simplify (F.unloop netlist))))
          | _ -> let netlist2 = E.collapse2 (I.index (T0.topsort (SL.simplify (F.unloop netlist)))) in
*)
          | SimpleLinesTop -> P.printNet ( (SL.simplify netlist))
          | Index -> P.printNet (I.index ( (SL.simplify netlist)))
          | _ -> let netlist2 = (E.collapse2 (E.regs (I.index ( (SL.simplify netlist))))) in
              match !action with
                Ast -> ()
              | Mast -> ()
              | Sast -> () 
              | Netlist -> ()
              | Forloops -> ()
              | SimpleLines -> ()
              | Netlist2 -> P.printNet2 netlist2
              | Topsort -> P.printNet2 (E.r2 (T.topsort netlist2))
              | LLVM_IR -> print_string (L.string_of_llmodule (C.translate (E.r2 (T.topsort netlist2))))
              | Compile -> let m = C.translate netlist2 in
	          Llvm_analysis.assert_valid_module m;
	          print_string (L.string_of_llmodule m)
