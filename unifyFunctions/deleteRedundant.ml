open InferDefs;;
open PrettyPrinter;;
open Syntax;;
open Aux;;

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++ Delete Redundant +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

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

