open InferDefs;;
open PrettyPrinter;;
open Syntax;;
open DeleteRedundant;;
open ExpressionUnification;;
open TypeUnification;;


(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++ Unify Constraints +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec unifyCS1 currentC rootCS =
	let subs = unifyTE1 (Tnode currentC.cnstrnt) rootCS in
	match currentC.link with
	  Cnode node ->
		unifyCS1 node rootCS; -5
	| Cempty -> -6
;;

(* ================================================= Unify Constraints =============================================== *)











(**
Test of delNOLs:

let extract e =
	let (Econ res) = e.eexp_node in
	res;;

print_string("ZZZZZZZZZZZZZZZZZZZZZZZZZ\n");;
let t1 = (tlabeled (tlabeled (tlabeled (tlabeled (tlabeled (tlabeled (texp (Tvar 1)) nol) zZZZZZZZZZ) nol) nol) yYYYYYYYYY) yYYYYYYYYY) in
let t2 = (tlabeled (tlabeled (tlabeled (tlabeled (texp (Tvar 1)) zZZZZZZZZZ) nol) yYYYYYYYYY) nol) in
let t3 = (tlabeled (tlabeled (tlabeled (tlabeled (texp (Tvar 1)) nol) zZZZZZZZZZ) yYYYYYYYYY) nol) in
let t4 = (tlabeled (tlabeled (tlabeled (tlabeled (texp (Tvar 1)) zZZZZZZZZZ) nol) nol) yYYYYYYYYY) in
let labelList = getAllLabels t4 in
let newLabelList = delNOLs labelList [] in
	print_type (texp (getUnlabeledPart t4)); print_newline();
	(*print_string (string_of_concrete_expr (extract (List.hd labelList)));*)
	print_string (string_of_labelList (List.map extract newLabelList));;
print_string("\nZZZZZZZZZZZZZZZZZZZZZZZZZ\n");;
*)





(**
(* fixme: check to see if these functions consider types in Tcon ... *)

let nextVar txp =
	match txp.tlink_node with
	  Tnode node ->
		begin match node.texp_node with
		  Tvar var -> var
		| _ -> -1 (* not a var *)
		end
	| Tempty -> -2 (* end of equation *)
;;

let rec isSingleTE te =
	match te.texp_node with
		  Tvar var ->
			(*print_string "The var "; print_int var;*)
			begin match te.tlink_node with
				  Tempty -> (*print_string " is single.";  print_newline();*) true
				| _ -> (*print_string " is NOt single.";  print_newline();*) false
			end
		| Tcon (sym, l) ->
			begin match sym with
			  Tarrow ->
				let ret1 = isSingleTE (List.hd l) and ret2 = isSingleTE (List.hd (List.tl l)) in
					if ( ret1 & ret2) then
						true
					else
						false
			| _ ->
				begin match te.tlink_node with
					  Tempty -> true
					| _ -> false
				end
			end
		| tlabeled (t, e) ->
			isSingleTE t;
;;

let rec delSingleInCS currentC parentC =
	match currentC.link with
		  Cnode nextC ->
			if (isSingleTE currentC.cnstrnt) then
				begin
					parentC.link <- currentC.link;
					begin match parentC.link with
						  Cnode newNextC ->
							delSingleInCS newNextC parentC
						| Cempty -> -36
					end
				end
			else
				delSingleInCS nextC currentC
		| Cempty ->
			if (isSingleTE currentC.cnstrnt) then
				begin parentC.link <- Cempty; -35 end
			else
				-34
;;


let rec delEqTE te =
	begin match te.texp_node with
	  Tvar var0 ->
		begin match te.tlink_node with
			  Tnode nextTnode ->
				let var1 = (nextVar te) in
					if (var0 == var1) then
						begin te.tlink_node <- nextTnode.tlink_node; -3 end
					else
						-4
			| Tempty ->
				-5
		end
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				delEqTE (List.hd l); delEqTE (List.hd (List.tl l)); -6
			| _ -> -7
		end
	| tlabeled (t, e) ->
		delEqTE t; -8
	| _ -> -9
	end;
	begin match te.tlink_node with
	  Tnode node ->
		delEqTE node; -10
	| Tempty -> -11
	end
;;

let rec delEqCS csNode =
	delEqTE csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			delEqCS node; -12
		| Cempty -> -13
;;


let rec subsTE tv1 tv2 te =
	begin match te.texp_node with
	  Tvar var0 ->
		if (var0 == tv1) then
			begin te.texp_node <- (Tvar tv2); -14 end
		else
			-15
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				subsTE tv1 tv2 (List.hd l); subsTE tv1 tv2 (List.hd (List.tl l)); -16
			| _ -> -17
		end
	| tlabeled (t, e) ->
		subsTE tv1 tv2 t; -18
	| _ -> -19
	end;
	begin match te.tlink_node with
	  Tnode node ->
		subsTE tv1 tv2 node; -20
	| Tempty -> -21
	end
;;

let rec subsCS tv1 tv2 csNode =
	subsTE tv1 tv2 csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			subsCS tv1 tv2 node; -22
		| Cempty -> -23
;;

let rec unifyVarInTE te cs =
	begin match te.texp_node with
	  Tvar var ->
		let next = nextVar te in
		if next > 0 then
			begin subsCS var next cs; -24 end
		else
			-25
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				unifyVarInTE (List.hd l) cs; unifyVarInTE (List.hd (List.tl l)) cs; -26
			| _ -> -27
		end
	| tlabeled (t, e) ->
		unifyVarInTE t cs; -28
	| _ -> -29
	end;
	begin match te.tlink_node with
	  Tnode node ->
		unifyVarInTE node cs; -30
	| Tempty -> -31
	end
;;

let rec unifyVarInCS cs1 root =
	unifyVarInTE cs1.cnstrnt root;
	match cs1.link with
		  Cnode node ->
			unifyVarInCS node root; -32
		| Cempty -> -33
;;

let unifyVarAndDelEq cs1 root =
	unifyVarInCS cs1 root;
	delEqCS cs1;;

let unifyVarAndDelEqAndDelSingle cs1 root =
	unifyVarInCS cs1 root;
	delEqCS cs1;
	let Cnode next = cs1.link in
	delSingleInCS  next root;;

let rec unifyTwoTE te1 te2 rootCS =
	match te1, te2 with
	  Tvar var1, Tvar var2 ->
		subsCS var1 var2 rootCS; -45
	| Tvar var, desc ->
		-46
	| desc, Tvar var ->
		-47
	| Tcon (Tint, []), Tcon (Tint, []) ->
		-48
	| Tcon (Tbool, []), Tcon (Tbool, []) ->
		-49
	| Tcon (Tlab, []), Tcon (Tlab, []) ->
		-50
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->
		-51
	| tlabeled (t1, e1), tlabeled (t2, e2) ->
		-52
	| _ ->
		-53
;;

let rec unifyTE currentTE rootCS =
	begin match currentTE.texp_node with
		  Tvar var ->
			-38
		| Tcon (sym, l)->
			begin match sym with
				  Tarrow ->
					unifyTE (List.hd l) rootCS; unifyTE (List.hd (List.tl l)) rootCS; -39
				| _ ->
					-40
			end;
		| tlabeled (t, e) ->
			unifyTE t rootCS; -41
		| _ -> -42
	end;
	begin match currentTE.tlink_node with
		  Tempty -> -43
		| Tnode node ->
			unifyTwoTE (currentTE.texp_node) (node.texp_node) rootCS; -44
	end
;;

let rec unifyCS currentC rootCS =
	unifyTE currentC.cnstrnt rootCS;
	match currentC.link with
	  Cnode node ->
		unifyCS node rootCS
	| Cempty -> -37
;;

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++ Unify +++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec unifyTwoTE te1 te2 subs =
	match te1, te2 with
	  Tvar var1, Tvar var2 ->
		(te1, te2)::subs;
	| Tvar var, desc ->
		(te1, te2)::subs;
	| desc, Tvar var ->
		(te2, te1)::subs;
	| Tcon (Tint, []), Tcon (Tint, []) ->
		subs;
	| Tcon (Tbool, []), Tcon (Tbool, []) ->
		subs;
	| Tcon (Tlab _, []), Tcon (Tlab _, []) ->
		subs;
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->
		let argUnify = unifyTwoTE t11.texp_node t21.texp_node subs in
		(* let unifiedT12 = applySubs (List.hd argUnify) rootCS  *) (* fixme: the line should correctly be implemented *)
		let resUnify = unifyTwoTE t21.texp_node t22.texp_node subs in
		argUnify@resUnify@subs;
	| tlabeled (t1, e1), tlabeled (t2, e2) ->
		let typeUnify = unifyTwoTE t1.texp_node t2.texp_node subs in
		(* let unifiedT12 = applySubs (List.hd argUnify) rootCS  *) (* fixme: the line should correctly be implemented *)
		let labUnify = unifyTwoE e1 e2 subs in
		typeUnify@labUnify@subs;
	| _ ->
		subs; (* fixme: Error should be implemented *)
;;

let rec unifyTE currentTE subs =
	let subs =
	begin match currentTE.texp_node with
		  Tvar var ->
			subs
		| Tcon (Tarrow, l)->
			let subs = unifyTE (List.hd l) subs in
			let subs = unifyTE (List.hd (List.tl l)) subs in
			subs
		| tlabeled (t, e) ->
			let subs = unifyTE t subs in (* fixme: we should also unify the types in e *)
			subs
		| _ -> subs
	end
	in
	begin match currentTE.tlink_node with
		  Tempty -> subs
		| Tnode nextTnode ->
			let subs = unifyTwoTE (currentTE.texp_node) (nextTnode.texp_node) subs in
			begin match nextTnode.tlink_node with
				  Tempty -> subs
				| Tnode _ ->
					unifyTE nextTnode subs
			end
	end
;;



let rec unifyCS currentC rootCS subs =
	let subs = unifyTE currentC.cnstrnt subs in
	match currentC.link with
	  Cnode node ->
		let subs = unifyCS node rootCS subs in
		subs
	| Cempty -> subs
;;
(* ======================================================= Unify ===================================================== *)


*)


