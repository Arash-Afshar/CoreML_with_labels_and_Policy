open InferDefs;;
open PrettyPrinter;;
open Syntax;;

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++ Delete Redundant +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let areExpEqual e1 e2 = true;; (* fixme *)

let areTvarsEqual (Tvar tv1) (Tvar tv2) =
	if (tv1 == tv2) then
		true
	else
		false
;;

let rec areTarrowsEqual (Tcon (Tarrow, [arg11; arg12])) (Tcon (Tarrow, [arg21; arg22])) =
	let firstArg  = areTexpEqual arg11.texp_node arg21.texp_node in
	let secondArg = areTexpEqual arg12.texp_node arg22.texp_node in
		firstArg & secondArg

and

areTexpEqual te1 te2 =
	match te1, te2 with
		  Tvar var1, Tvar var2 ->
			areTvarsEqual te1 te2
		| Tcon (Tarrow, _), Tcon (Tarrow, _) ->
			areTarrowsEqual te1 te2
		| Tcon (Tint, _), Tcon (Tint, _) -> true
		| Tcon (Tbool, _), Tcon (Tbool, _) -> true
		| Tcon (Tlab _, _), Tcon (Tlab _, _) -> true (* fixme *)
		| Tlabeled (t1, e1), Tlabeled (t2, e2) ->
			let t = areTexpEqual t1.texp_node t2.texp_node in
			let e = areExpEqual e1 e2 in
				t & e
		| _ -> false
;;

let rec delEqTE (Tnode te) =
	match te.tlink_node with
	  Tnode nextTnode ->
		if (areTexpEqual te.texp_node nextTnode.texp_node) then
			begin
				te.tlink_node <- nextTnode.tlink_node;
				delEqTE (Tnode te);
				-3
			end
		else
			delEqTE te.tlink_node
	| Tempty ->
		-3
;;

let rec traverseDelEqTE (Tnode te) =
	match te.texp_node with
	  Tcon (Tarrow, [t1;t2]) ->
		traverseDelEqTE (Tnode t1); traverseDelEqTE (Tnode t2);
		delEqTE (Tnode te) 
	| Tlabeled (t, e) ->
		(* fixme: delete equal e *)
		traverseDelEqTE (Tnode t);
		delEqTE (Tnode te)
	| _ ->
		delEqTE (Tnode te)
;;



let rec delEqCS csNode =
	traverseDelEqTE (Tnode csNode.cnstrnt);
	match csNode.link with
		  Cnode node ->
			delEqCS node; -12
		| Cempty -> -13
;;



let rec isSingleTE te =
	match te.texp_node with
		  Tcon (Tarrow, l) ->
			let inner =
				let ret1 = isSingleTE (List.hd l) and ret2 = isSingleTE (List.hd (List.tl l)) in
					if ( ret1 & ret2) then
						true
					else
						false
			in
			begin match te.tlink_node with
				  Tempty ->
					if inner then
						true
					else
						false
				| _ -> false
			end;
		| Tlabeled (t, e) ->
			let inner = isSingleTE t in
			begin match te.tlink_node with
				  Tempty ->
					if inner then
						true
					else
						false
				| _ -> false
			end;
		| _ ->
			begin match te.tlink_node with
				  Tempty -> true
				| _ -> false
			end
;;

let rec delSingleInCS currentC parentC = (* fixme: a single constrtaint should not be deleted if it is inside an arrow or lab *)
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

(* ================================================== Delete Redundant =============================================== *)

