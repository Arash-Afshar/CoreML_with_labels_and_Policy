open InferDefs;;
open PrettyPrinter;;
open Syntax;;
open ExpressionUnification;;
open DeleteRedundant;;
open DelNOLs;;

exception UnunifyingConstraints of string;;

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++ Subs Var with Var +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec subsVarVar (tv1, tv2) te =
	begin match te.texp_node with
	  Tvar var0 ->
		if (var0 == tv1) then
			begin te.texp_node <- (Tvar tv2); -14 end
		else
			-15
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				subsVarVar (tv1, tv2) (List.hd l); subsVarVar (tv1, tv2) (List.hd (List.tl l)); -16
			| _ -> -17
		end
	| Tlabeled (t, e) ->
		subsVarVar (tv1, tv2) t; -18
	| _ -> -19
	end;
	begin match te.tlink_node with
	  Tnode node ->
		subsVarVar (tv1, tv2) node; -20
	| Tempty -> -21
	end
;;

let rec subsVarVarCS (tv1, tv2) csNode =
	subsVarVar (tv1, tv2) csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			subsVarVarCS (tv1, tv2) node; -22
		| Cempty -> -23
;;
(* ================================================ Subs Var with Var ================================================ *)


(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++ Subs Var with other ++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec subsVarOther (tv, other) te =
	begin match te.texp_node with
	  Tvar var0 ->
		if (var0 == tv) then
			begin te.texp_node <- other; -14 end
		else
			-15
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				subsVarOther (tv, other) (List.hd l); subsVarOther (tv, other) (List.hd (List.tl l)); -16
			| _ -> -17
		end
	| Tlabeled (t, e) ->
		subsVarOther (tv, other) t; -18
	| _ -> -19
	end;
	begin match te.tlink_node with
	  Tnode node ->
		subsVarOther (tv, other) node; -20
	| Tempty -> -21
	end
;;

let rec subsVarOtherCS (tv, other) csNode =
	subsVarOther (tv, other) csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			subsVarOtherCS (tv, other) node; -22
		| Cempty -> -23
;;
(* =============================================== Subs Var with other =============================================== *)

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++ apply substitute +++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

let rec applySubsCS (te1, te2) csNode =
	match te1, te2 with
	  Tvar -1, Tvar -1 ->
		-44
	| Tvar var1, Tvar var2 ->
		subsVarVarCS (var1, var2) csNode; -45
	| Tvar var, other ->
		subsVarOtherCS (var, other) csNode; -46
	| other, Tvar var ->
		subsVarOtherCS (var, other) csNode; -47
	| Tcon (Tint, []), Tcon (Tint, []) ->
		-48
	| Tcon (Tbool, []), Tcon (Tbool, []) ->
		-49
	| Tcon (Tlab l1, []), Tcon (Tlab l2, []) ->
		(* fixme: expression substitution *)
		applyExpSubsCS (l1.eexp_node, l2.eexp_node) csNode;
		-50
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->
		-51 (* fixme: error *)
	| Tlabeled (t1, e1), Tlabeled (t2, e2) ->
		-52 (* fixme: error *)
	| _ ->
		-53 (* fixme: error *)
;;


let rec applySubsList subsSet csNode =
	match subsSet with
		  [] -> -24
		| _  ->
			begin
			applySubsCS (List.hd subsSet) csNode;
			(*applyExpSubs (List.hd subsSet) csNode;*)
			match csNode.link with
				  Cnode node ->
					applySubsList (List.tl subsSet) csNode; -22
				| Cempty -> -23
			end
;;

(* ================================================== apply substitute =============================================== *)



(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++ Unify Types ++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(*
Func4: cleanUp functions
1. apply the subs to all TEs in CS
2. delete redundants
3. delete singles

Func0: unification function for expressions
this function may be two functions, like Func1 and Func2

Func1: a function that unifies two TEs:
	1. compare the tw
	1.1. var arrow  | arrow  var ->
	1.1.1. call Func2 on arrow arg1
	1.1.2. call Func2 on arrow arg2
	1.1.3. return subs of var with arrow (be careful, var may have been changed because of the substitution)
	1.2. var labeld | labeld var ->
	1.2.1. call Func2 on labeld type
	1.2.2. call Func0 on labeld label
	1.2.3. return subs of var with labeld (be careful, var may have been changed because of the substitution)
	1.3. var var    -> straight forward
	1.4. var others -> straight forward
	1.5. arrow arrow   ->
	1.5.1. call Func2 on arrow1 arg1
	1.5.2. call Func2 on arrow1 arg2
	1.5.3. call Func2 on arrow2 arg1
	1.5.4. call Func2 on arrow2 arg2
	1.5.5. call Func1 on (arrow1 arg1) and (arrow2 arg1)
	1.5.6. call Func4
	1.5.7. call Func1 on (arrow1 arg2) and (arrow2 arg2)
	1.5.8. call Func4
	1.6. labeld labeld ->
	1.6.1. call Func2 on labeld1 type
	1.6.2. call Func2 on labeld2 type
	1.6.3. call Func0 on labeld1 label
	1.6.4. call Func0 on labeld2 label
	1.6.5. call Func1 on (labeld1 type) and (labeld2 type)
	1.6.6. call Func4
	1.6.7. call Func0 on (labeld1 label) and (labeld2 label)
	1.6.8. call Func4
	1.7. other other   -> trivial
	1.8. not the same  -> error

Func2: a function that traverses TEs:
[for each two] do the following
1. call Func1
2. call Func4
3. call Func2 (recursive)

Func3: a function that traverses CS
*)
let applyAndClean tSubs cs =
	let _ = applySubsCS tSubs cs in (* fixme *)
	(*let _ = applyExpSubsCS eSubs cs in (** fixme *)*)
	let _ = delEqCS cs in
	let _ = delNOLsCS in
	(*let _ = delSingleInCS cs cs in*)
		-2
;;

(*
let rec unifyE1 currentE rootCS = [];;
let rec unifyTwoE1 e1 e2 rootCS = [];;
*)

let rec unifyTwoTE1 te1 te2 rootCS =
	match te1, te2 with
	(*  Tvar var, Tcon (Tarrow, [t1; t2]) ->
		unifyTE1 (Tnode t1) rootCS;
		unifyTE1 (Tnode t2) rootCS;
		(te1, te2)
	| Tcon (Tarrow, [t1; t2]), Tvar var ->
		unifyTE1 (Tnode t1) rootCS;
		unifyTE1 (Tnode t2) rootCS;
		(te2, te1)
	| Tvar var, Tlabeled (t, e) ->
		unifyTE1 (Tnode t) rootCS;
		(*unifyExp e rootCS;*)
		(te1, te2)
	| Tlabeled (t, e), Tvar var ->
		unifyTE1 (Tnode t) rootCS;
		(*unifyExp e rootCS;*)
		(te2, te1)
	|*) Tvar var1, Tvar var2 ->
		(te1, te2);
	| Tvar var, desc ->
		(te1, te2);
	| desc, Tvar var ->
		(te2, te1);
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->	
		(*unifyTE1 (Tnode t11) rootCS;
		unifyTE1 (Tnode t12) rootCS;
		unifyTE1 (Tnode t21) rootCS;
		unifyTE1 (Tnode t22) rootCS;*)
		let argSubs = unifyTwoTE1 t11.texp_node t21.texp_node rootCS in
		let _ = applyAndClean argSubs rootCS in
		let resSubs = unifyTwoTE1 t12.texp_node t22.texp_node rootCS in
		resSubs
	| Tlabeled (t1, (Enode e1)), Tlabeled (t2, (Enode e2)) ->
		(*unifyTE1 (Tnode t1) rootCS;
		unifyTE1 (Tnode t2) rootCS;*)
		(*unifyExp  e1 rootCS;
		unifyExp  e2 rootCS;*)
		let typeSubs = unifyTwoTE1 t1.texp_node t2.texp_node rootCS in
		let _ = applyAndClean typeSubs rootCS in
		(*let labSubs = unifyTwoE1 e1 e2 rootCS in*)
		let labSubs = unifyTwoE e1.eexp_node e2.eexp_node in
		let _ = applyExpSubsCS labSubs rootCS in
		typeSubs
	| Tcon (Tint, []), Tcon (Tint, []) ->
		(Tvar (-1), Tvar (-1));
	| Tcon (Tbool, []), Tcon (Tbool, []) ->
		(Tvar (-1), Tvar (-1));
	| Tcon (Tlab l1, []), Tcon (Tlab l2, []) ->
		(* fixme: expression unification *)
		let labSubs = unifyTwoE l1.eexp_node l2.eexp_node in
		let _ = applyExpSubsCS labSubs rootCS in
		(Tvar (-1), Tvar (-1));
	| _ ->
		raise (UnunifyingConstraints ((string_of_short_texp (texp te1))^" != "^(string_of_short_texp (texp te2))));
and
unifyTE1 currentTE rootCS =
	match currentTE with
		  Tempty ->
			-3
		| Tnode node ->
			begin match node.texp_node with
			  Tcon (Tarrow, [t1;t2]) ->
				unifyTE1 (Tnode t1) rootCS; unifyTE1 (Tnode t1) rootCS;
			| Tlabeled (t, e) ->
				unifyTE1 (Tnode t) rootCS;
			| _ ->
				2
			end;
			begin match node.tlink_node with	
				  Tempty ->
					(*delSingleInCS rootCS rootCS;*) -4
				| Tnode nextTnode ->
					let subs = unifyTwoTE1 (node.texp_node) (nextTnode.texp_node) rootCS in
					begin match subs with
						  (Tvar (-1), Tvar (-1)) -> (* for optimization, we only delete  equal *)
							delEqCS rootCS; (*delSingleInCS rootCS rootCS;*) -5
						(*fixme: error handling is done in unifyTwoTE1 *)	
						| _  ->
							applyAndClean subs rootCS; -6
					end;
					unifyTE1 (node.tlink_node) rootCS;
					(*unifyTE1 currentTE rootCS;*) (* fixme: think more about this *)
			end
			
;;
(* ==================================================== Unify Types ================================================== *)

