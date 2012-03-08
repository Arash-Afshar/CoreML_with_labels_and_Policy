open InferDefs;;
open PrettyPrinter;;
open Syntax;;
open ExpressionUnification;;
(*open DeleteRedundant;;*)
open DelNOLs;;

exception UnunifyingConstraints of string;;

(*|(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)*)
(*|(* ++++++++++++++++++++++++++++++++++ Subs Var with Var                    *)                                            *)
(*|(* +++++++++++++++++++++++++++++++                                         *)                                            *)
(*|(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)*)
(*|let rec subsVarVar (tv1, tv2) te =                                                                                       *)
(*|	begin match te.texp_node with                                                                                          *)
(*|			Tvar var0 ->                                                                                                       *)
(*|				if (var0 == tv1) then                                                                                            *)
(*|					te.texp_node <- (Tvar tv2)                                                                                     *)
(*|		| Tcon (Tarrow, [t1; t2]) ->                                                                                         *)
(*|				subsVarVar (tv1, tv2) t1; subsVarVar (tv1, tv2) t2;                                                              *)
(*|		| Tlabeled (t, e) ->                                                                                                 *)
(*|				subsVarVar (tv1, tv2) t;                                                                                         *)
(*|		| _ -> ()                                                                                                            *)
(*|	end;                                                                                                                   *)
(*|	begin match te.tlink_node with                                                                                         *)
(*|			Tnode node ->                                                                                                      *)
(*|				subsVarVar (tv1, tv2) node;                                                                                      *)
(*|		| Tempty -> ()                                                                                                       *)
(*|	end                                                                                                                    *)
(*|;;                                                                                                                       *)
(*|                                                                                                                         *)
(*|let rec subsVarVarCS (tv1, tv2) csNode =                                                                                 *)
(*|	subsVarVar (tv1, tv2) csNode.cnstrnt;                                                                                  *)
(*|	match csNode.link with                                                                                                 *)
(*|		Cnode node ->                                                                                                        *)
(*|			subsVarVarCS (tv1, tv2) node                                                                                       *)
(*|	| Cempty -> ()                                                                                                         *)
(*|;;                                                                                                                       *)
(*|(* ================================================ Subs Var with Var      *)                                            *)
(*|(* ================================================                        *)                                            *)

(*|+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*|++++++++++++++++++++++++++++++++++++++++++++++++ Subs Var with other ++++++++++++++++++++++++++++++++++++++++++++++*)
(*|+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
let rec subsVarOther (tv, other) te =
	begin match te.texp_node with
			Tvar var0 ->
				if (var0 == tv) then
					te.texp_node <- other
		| Tcon (Tarrow, [t1; t2]) ->
				subsVarOther (tv, other) t1; subsVarOther (tv, other) t2;
		| Tlabeled (t, e) ->
				subsVarOther (tv, other) t;
		| _ -> ()
	end;
	begin match te.tlink_node with
			Tnode node ->
				subsVarOther (tv, other) node
		| Tempty -> ()
	end
;;

let rec subsVarOtherCS (tv, other) csNode =
	subsVarOther (tv, other) csNode.cnstrnt;
	match csNode.link with
		Cnode node ->
			subsVarOtherCS (tv, other) node
	| Cempty -> ()
;;
(*|=============================================== Subs Var with other ===============================================*)

(*|+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*|++++++++++++++++++++++++++++++++++++++++++++++++ apply substitute +++++++++++++++++++++++++++++++++++++++++++++++++*)
(*|+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

let rec applySubsCS (te1, te2) csNode =
	match te1, te2 with
		Tvar -1, Tvar -1 ->
			()
	| Tvar var1, Tvar var2 ->
			subsVarOtherCS (var1, te2) csNode
	| Tvar var, other ->
			subsVarOtherCS (var, other) csNode
	| other, Tvar var ->
			subsVarOtherCS (var, other) csNode
	| Tcon (Tint, []), Tcon (Tint, []) ->
			()
	| Tcon (Tbool, []), Tcon (Tbool, []) ->
			()
	| Tcon (Tlab l1, []), Tcon (Tlab l2, []) ->
	(* fixme: expression substitution *)
			applyExpSubsCS (l1.eexp_node, [l2.eexp_node]) csNode
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->
			() (* fixme: error *)
	| Tlabeled (t1, e1), Tlabeled (t2, e2) ->
			() (* fixme: error *)
	| _ ->
			() (* fixme: error *)
;;

let rec applySubsList subsSet csNode =
	match subsSet with
		[] -> ()
	| _ ->
			begin
				applySubsCS (List.hd subsSet) csNode;
				(* applyExpSubs (List.hd subsSet) csNode; *)
				match csNode.link with
					Cnode node ->
						applySubsList (List.tl subsSet) csNode
				| Cempty -> ()
			end
;;

(*|================================================== apply substitute ===============================================*)

let rec makeEqualList labelList1 labelList2 =
	if (List.length labelList1) == 0 then
		[]
	else
		((List.hd labelList1).eexp_node, [(List.hd labelList2).eexp_node]):: (makeEqualList (List.tl labelList1) (List.tl labelList2))
;;

let rec makeEqual labelList label =
	if (List.length labelList) == 0 then
		[]
	else
		((List.hd labelList).eexp_node, [label.eexp_node]):: (makeEqual (List.tl labelList) label)
;;

let rec isVar label =
	match label.eexp_node with
		Evar v ->
			true
	| _ ->
			false
;;

let rec isNOL label =
	match label.eexp_node with
		Econ (Const { name = Lab "noLab"; arity = 0; constr = false }) ->
			true
	| _ ->
			false
;;

let decide_Which_Label_Is_Equal_To_Which_Label labelList1 labelList2 =
	let length1 = List.length labelList1 in
	let length2 = List.length labelList2 in
	if (length1 == length2) then
		makeEqualList labelList1 labelList2
	else if (length1 == 1) then
		let label = List.hd labelList1 in
		if (isNOL label) then
			makeEqual labelList2 label
		else if (isVar label) then
			begin
				(*| print_newline(); print_string                                  *)
				(*| "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY"; print_newline();         *)
				(*| print_string (string_of_label label); print_newline();         *)
				(*| print_string (string_of_labelList labelList2); print_newline();*)
				(*| print_string "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY";             *)
				(*| print_newline();                                               *)
				[label.eexp_node, List.map extractEdesc labelList2]
			end
		else
			(* fixme: error *)
			[(Evar (-1), [Evar (-1)])]
	else if (length2 == 1) then
		let label = List.hd labelList2 in
		if (isNOL label) then
			makeEqual labelList1 label
		else if (isVar label) then
			begin
				(*| print_newline(); print_string                                  *)
				(*| "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"; print_newline();         *)
				(*| print_string (string_of_label label); print_newline();         *)
				(*| print_string (string_of_labelList labelList1); print_newline();*)
				(*| print_string "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";             *)
				(*| print_newline();                                               *)
				[label.eexp_node, List.map extractEdesc labelList1]
			end
		else
			(* fixme: error *)
			[(Evar (-1), [Evar (-1)])]
	else
		(* fixme: error *)
		[(Evar (-1), [Evar (-1)])]
;;

(*|+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*|++++++++++++++++++++++++++++++++++++++++++++++++++++ Unify Types ++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*|+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(*|Func4: cleanUp functions                                                                                   *)
(*|1. apply the subs to all TEs in CS                                                                         *)
(*|2. delete redundants                                                                                       *)
(*|3. delete singles                                                                                          *)
(*|                                                                                                           *)
(*|Func0: unification function for expressions                                                                *)
(*|this function may be two functions, like Func1 and Func2                                                   *)
(*|                                                                                                           *)
(*|Func1: a function that unifies two TEs:                                                                    *)
(*|	1. compare the tw                                                                                        *)
(*|	1.1. var arrow  | arrow  var ->                                                                          *)
(*|	1.1.1. call Func2 on arrow arg1                                                                          *)
(*|	1.1.2. call Func2 on arrow arg2                                                                          *)
(*|	1.1.3. return subs of var with arrow (be careful, var may have been changed because of the substitution) *)
(*|	1.2. var labeld | labeld var ->                                                                          *)
(*|	1.2.1. call Func2 on labeld type                                                                         *)
(*|	1.2.2. call Func0 on labeld label                                                                        *)
(*|	1.2.3. return subs of var with labeld (be careful, var may have been changed because of the substitution)*)
(*|	1.3. var var    -> straight forward                                                                      *)
(*|	1.4. var others -> straight forward                                                                      *)
(*|	1.5. arrow arrow   ->                                                                                    *)
(*|	1.5.1. call Func2 on arrow1 arg1                                                                         *)
(*|	1.5.2. call Func2 on arrow1 arg2                                                                         *)
(*|	1.5.3. call Func2 on arrow2 arg1                                                                         *)
(*|	1.5.4. call Func2 on arrow2 arg2                                                                         *)
(*|	1.5.5. call Func1 on (arrow1 arg1) and (arrow2 arg1)                                                     *)
(*|	1.5.6. call Func4                                                                                        *)
(*|	1.5.7. call Func1 on (arrow1 arg2) and (arrow2 arg2)                                                     *)
(*|	1.5.8. call Func4                                                                                        *)
(*|	1.6. labeld labeld ->                                                                                    *)
(*|	1.6.1. call Func2 on labeld1 type                                                                        *)
(*|	1.6.2. call Func2 on labeld2 type                                                                        *)
(*|	1.6.3. call Func0 on labeld1 label                                                                       *)
(*|	1.6.4. call Func0 on labeld2 label                                                                       *)
(*|	1.6.5. call Func1 on (labeld1 type) and (labeld2 type)                                                   *)
(*|	1.6.6. call Func4                                                                                        *)
(*|	1.6.7. call Func0 on (labeld1 label) and (labeld2 label)                                                 *)
(*|	1.6.8. call Func4                                                                                        *)
(*|	1.7. other other   -> trivial                                                                            *)
(*|	1.8. not the same  -> error                                                                              *)
(*|                                                                                                           *)
(*|Func2: a function that traverses TEs:                                                                      *)
(*|[for each two] do the following                                                                            *)
(*|1. call Func1                                                                                              *)
(*|2. call Func4                                                                                              *)
(*|3. call Func2 (recursive)                                                                                  *)
(*|                                                                                                           *)
(*|Func3: a function that traverses CS                                                                        *)

let applyAndClean tSubs cs =
	applySubsList tSubs cs;
(*|	applySubsCS tSubs cs*)
(*|let _ = applySubsCS tSubs cs in*) (* fixme                            *)
(*| let _ = applyExpSubsCS eSubs cs in (** fixme *) let _ = delEqCS cs in*)
(*| let _ = delNOLsCS in let _ = delSingleInCS cs cs in                  *)
;;

(*|let rec unifyE1 currentE rootCS = [];; let rec unifyTwoE1 e1 e2 rootCS =*)
(*|	[]                                                                    *)
(*|;;                                                                      *)

let rec unifyTwoTE1 te1 te2 rootCS =
	match te1, te2 with
	(*|	 Tvar var, Tcon (Tarrow, [t1; t2]) -> *)
	(*|		unifyTE1 (Tnode t1) rootCS;         *)
	(*|		unifyTE1 (Tnode t2) rootCS;         *)
	(*|		(te1, te2)                          *)
	(*|	| Tcon (Tarrow, [t1; t2]), Tvar var ->*)
	(*|		unifyTE1 (Tnode t1) rootCS;         *)
	(*|		unifyTE1 (Tnode t2) rootCS;         *)
	(*|		(te2, te1)                          *)
	(*|	| Tvar var, Tlabeled (t, e) ->        *)
	(*|		unifyTE1 (Tnode t) rootCS;          *)
	(*|		(*unifyExp e rootCS;*)              *)
	(*|		(te1, te2)                          *)
	(*|	| Tlabeled (t, e), Tvar var ->        *)
	(*|		unifyTE1 (Tnode t) rootCS;          *)
	(*|		(*unifyExp e rootCS;*)              *)
	(*|		(te2, te1)                          *)
	(*|	|                                     *)
		Tvar var1, Tvar var2 ->
			[(te1, te2)];
	| Tvar var, desc ->
			[(te1, te2)];
	| desc, Tvar var ->
			[(te2, te1)];
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->
	(*|		unifyTE1 (Tnode t11) rootCS;*)
	(*|		unifyTE1 (Tnode t12) rootCS;*)
	(*|		unifyTE1 (Tnode t21) rootCS;*)
	(*|		unifyTE1 (Tnode t22) rootCS;*)
			let argSubs = unifyTwoTE1 t11.texp_node t21.texp_node rootCS in
			let _ = applyAndClean argSubs rootCS in
			let resSubs = unifyTwoTE1 t12.texp_node t22.texp_node rootCS in
			argSubs@resSubs
	| Tlabeled (t1, lList1), Tlabeled (t2, lList2) ->
	(*|		unifyTE1 (Tnode t1) rootCS;*)
	(*|		unifyTE1 (Tnode t2) rootCS;*)
	(*|		unifyExp  e1 rootCS;       *)
	(*|		unifyExp  e2 rootCS;       *)
			let typeSubs = unifyTwoTE1 t1.texp_node t2.texp_node rootCS in
			let _ = applyAndClean typeSubs rootCS in
			(*|		let labSubs = unifyTwoE1 e1 e2 rootCS in            *)
			(*|		let labSubs = unifyTwoE e1.eexp_node e2.eexp_node in*)
			let labSubs = decide_Which_Label_Is_Equal_To_Which_Label lList1 lList2 in
			let _ = applyExpSubsList labSubs rootCS in
			typeSubs
	| Tcon (Tint, []), Tcon (Tint, []) ->
			[(Tvar (-1), Tvar (-1))];
	| Tcon (Tbool, []), Tcon (Tbool, []) ->
			[(Tvar (-1), Tvar (-1))];
	| Tcon (Tlab l1, []), Tcon (Tlab l2, []) ->
	(* fixme: expression unification *)
			let labSubs = unifyTwoE l1.eexp_node l2.eexp_node in
			let _ = applyExpSubsCS labSubs rootCS in
			[(Tvar (-1), Tvar (-1))];
	| _ ->
			raise (UnunifyingConstraints ((string_of_type (teyxp te1))^" != "^(string_of_type (teyxp te2))));
;;

let rec unifyTE1 rootCS node =
			let subs12 =
				begin match node.texp_node with
						Tcon (Tarrow, [t1; t2]) ->
							let subs1 = unifyTE1 rootCS t1 in
							let subs2 = unifyTE1 rootCS t1 in
							subs1 @ subs2
					| Tlabeled (t, e) ->
							unifyTE1 rootCS t
					| _ ->
							[]
				end in
			let subs3 =
				begin match node.tlink_node with
						Tempty ->
					(* delSingleInCS rootCS rootCS; *)
							[]
					| Tnode nextTnode ->
							let subs = unifyTwoTE1 (node.texp_node) (nextTnode.texp_node) rootCS in
							let _ =
								begin match subs with
										[(Tvar (-1), Tvar (-1))] -> (* for optimization, we only delete  equal *)
											()
									(*| delEqCS rootCS; delSingleInCS rootCS rootCS; fixme: error*)
									(*| handling is done in unifyTwoTE1                          *)
									| _ ->
											applyAndClean subs rootCS
								end in
							let subs4 = unifyTE1 rootCS nextTnode in
							subs @ subs4
					(*|unifyTE1 currentTE rootCS;   *)
					(*| fixme: think more about this*)
				end in
			subs12 @ subs3
;;
(*|==================================================== Unify Types ==================================================*)

