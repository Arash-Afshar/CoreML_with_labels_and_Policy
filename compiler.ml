open PrettyPrinter;;
open Syntax;;
open Reduce;;
open ConstraintPolyInfer;;
open ConstraintUnify;;
open InferDefs;;



let _ =
	try
		let lexbuf = Lexing.from_channel (open_in Sys.argv.(1) ) in
		while true do
			let result = Parser.main Lexer.token lexbuf in
			let t = tevar() and cs = csInit() in
			(*let etPair = (ETnode initExpTypePair) in*)
			let _ = genConstrSet result t cs (*etPair*) in
			(*print_string("concrete syntax of expression="); print_newline(); print_concrete_expr(result); print_newline(); print_newline();
			print_string("abstract syntax of expression="); print_newline(); print_abstract_expr(result); print_newline(); print_newline();*)
			print_string("constrints="); print_newline();  print_constraint_set (Cnode cs); print_newline(); print_newline();
			(*print_string("expressions and types ="); print_newline();  print_expType (etPair); print_newline(); print_newline();*)
			(*print_string("reduced constrints="); print_newline();  print_constraint_set (unifyVarAndDelEq cs cs; Cnode cs); print_newline(); print_newline();
			print_string("reduced constrints="); print_newline();  print_constraint_set (unifyVarAndDelEqAndDelSingle cs cs; Cnode cs); print_newline(); print_newline();*)
			let _ = unifyCS1 cs cs in
			(*let ret1 = unifyCS cs cs [] in
			let ret2 = applySubsCS ret1 cs in
			print_string("substitutions="); print_newline(); print_subsSet ret1; print_newline();
			print_string("reduced constraints="); print_newline(); print_constraint_set (Cnode cs); print_newline(); print_newline();
			let _ = delEqCS cs in
			let _ = delSingleInCS cs cs in
			(*let _ = delSingleInCS cs cs in *)
			print_string("redundant deleted constrints="); print_newline();  print_constraint_set (Cnode cs); print_newline(); print_newline();*)
			print_string("reduced constraints=");
			print_newline();
			print_constraint_set (Cnode cs);
			print_newline(); print_newline();
			print_string("type of expression="); print_newline(); print_type t; print_newline(); print_newline();
			(*print_string("value of expression="); print_newline(); print_answer(eval [] "code" result); print_newline(); print_newline();*)
			flush stdout
		done
	with Lexer.Eof ->
		exit 0;;


