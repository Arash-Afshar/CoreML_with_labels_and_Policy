open InferDefs;;
open PrettyPrinter;;
open Syntax;;


(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++ Delete NOLs ++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

let rec addBatchLabels labelList labelAcceptingPart =
	let length = (List.length labelList) in
	if (length != 0) then
		(tlabaled labelAcceptingPart (List.hd labelList))
;;

let rec addLabelsToType labelList unlabeledPart parent =
	te.texp_node <- (addBatchLabels labelList unlabeledPart)
;; 

let rec delNOLs labelList resultList =
	let length = (List.length labelList) in
	if (length == 0) then
		resultList
	else
	begin
		let hd = (List.hd labelList) in
		if hd == nol then
			delNOLs (List.tl labelList) resultList
		else
			delNOLs (List.tl labelList) (resultList @ [hd])
	end
;;

let rec delNOLsTE (Tnode te) =
	match te.texp_node with
	  Tlabeled (t, e) ->
		let labelList = getAllLabels te in
		let newLabelList = delNOLs labelList in
		let unlabeledPart = getUnlabeledPart te in
		addLabelsToType newLabelList unlabeledPart te
	| _ ->
		-4
;;

let rec traverseDelNOLsTE (Tnode te) =
	match te.texp_node with
	  Tcon (Tarrow, [t1;t2]) ->
		traverseDelNOLsTE (Tnode t1); traverseDelNOLsTE (Tnode t2);
	| Tlabeled (t, e) ->
		(* fixme: delete equal e *)
		traverseDelNOLsTE (Tnode t);
		delNOLsTE (Tnode te);
		-15
	| _ ->
		-14
;;


let rec delNOLsCS csNode =
	traverseDelNOLsTE (Tnode csNode.cnstrnt);
	match csNode.link with
		  Cnode node ->
			delNOLsCS node; -12
		| Cempty -> -13
;;

let extract e =
	let (Econ res) = e.eexp_node in
	res;;

(* ==================================================== Delete NOLs ================================================== *)


exception UnunifyingConstraints of string;;
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++ Unify Expressions +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

let rec unifyTwoE e1 e2 =
	begin match e1, e2 with
	  Evar v1, Evar v2 ->
		(Evar v1, Evar v2)
	| Evar v, Econ expr ->
		unifyExp expr;
		(e1, e2)
	| Econ expr, Evar v ->
		unifyExp expr;
		(e2, e1)
	| Econ expr1, Econ expr2 ->
		begin match expr1, expr2 with
		  Var v1, Var v2 ->
			begin
				if (v1 != v2) then
					raise (UnunifyingConstraints ("expression variables "^v1^" and "^v2^" cannot be unified!")) (* fixme: it might be wrong!*)
				else
					(Evar (-1), Evar (-1)) (* means: nothing to do *)
			end
		(*| Var v, exp -> 1
		| exp, Var v -> 1*)
		| Fun(v1, e1), Fun(v2, e2) ->
			begin 
				let subs1 = unifyTwoE (Econ (Var v1)) (Econ (Var v2)) in
				(* substitute the changes *)
				let subs2 = unifyTwoE (Econ e1) (Econ e2) in
				subs2
			end
		| App(e1, e2), App(e3, e4) ->
			begin 
				let subs1 = unifyTwoE (Econ e1) (Econ e3) in
				(* substitute the changes *)
				let subs2 = unifyTwoE (Econ e2) (Econ e4) in
				subs2
			end
		| Let(vl1, e1, e2), Let(vl2, e3, e4) -> (Evar (-1), Evar (-1))
		| LetP(vl1, e1, e2), LetP(vl2, e3, e4) -> (Evar (-1), Evar (-1))
		| Const {name = Int n1;    arity = 0; constr = true}, Const {name = Int n2;    arity = 0; constr = true} -> (Evar (-1), Evar (-1))
		| Const {name = Bool b1;   arity = 0; constr = true}, Const {name = Bool b2;   arity = 0; constr = true} -> (Evar (-1), Evar (-1))
		| _ -> (Evar (-2), Evar (-2))
		end
	end
and
unifyExp e = 1;;
(* ================================================= Unify Expressions ============================================== *)


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
(* ++++++++++++++++++++++++++++++++++++++++++++++++++ Delete Redundant +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let areExpEqual e1 e2 = true;;

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


(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++ apply substitute +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec applySubs (te1, te2) csNode =
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
	| Tcon (Tlab _, []), Tcon (Tlab _, []) ->
		(* fixme: expression substitution *)
		-50
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->
		-51 (* fixme: error *)
	| Tlabeled (t1, e1), Tlabeled (t2, e2) ->
		-52 (* fixme: error *)
	| _ ->
		-53 (* fixme: error *)
;;

let rec applySubsCS subsSet csNode =
	match subsSet with
		  [] -> -24
		| _  ->
			begin
			applySubs (List.hd subsSet) csNode;
			match csNode.link with
				  Cnode node ->
					applySubsCS (List.tl subsSet) csNode; -22
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
	1. compare the two
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
let applyAndClean subs cs =
	let _ = applySubs subs cs in
	let _ = delEqCS cs in
	(*let _ = delSingleInCS cs cs in*)
		-2
;;

let rec unifyE1 currentE rootCS = [];;
let rec unifyTwoE1 e1 e2 rootCS = [];;


let rec unifyTwoTE1 te1 te2 rootCS =
	match te1, te2 with
	  Tvar var, Tcon (Tarrow, [t1; t2]) ->
		unifyTE1 (Tnode t1) rootCS;
		unifyTE1 (Tnode t2) rootCS;
		(te1, te2)
	| Tcon (Tarrow, [t1; t2]), Tvar var ->
		unifyTE1 (Tnode t1) rootCS;
		unifyTE1 (Tnode t2) rootCS;
		(te2, te1)
	| Tvar var, Tlabeled (t, e) ->
		unifyTE1 (Tnode t) rootCS;
		unifyE1  e rootCS;
		(te1, te2)
	| Tlabeled (t, e), Tvar var ->
		unifyTE1 (Tnode t) rootCS;
		unifyE1  e rootCS;
		(te2, te1)
	| Tvar var1, Tvar var2 ->
		(te1, te2);
	| Tvar var, desc ->
		(te1, te2);
	| desc, Tvar var ->
		(te2, te1);
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->	
		unifyTE1 (Tnode t11) rootCS;
		unifyTE1 (Tnode t12) rootCS;
		unifyTE1 (Tnode t21) rootCS;
		unifyTE1 (Tnode t22) rootCS;
		let argSubs = unifyTwoTE1 t11.texp_node t21.texp_node rootCS in
		let _ = applyAndClean argSubs rootCS in
		let resSubs = unifyTwoTE1 t12.texp_node t22.texp_node rootCS in
		resSubs
	| Tlabeled (t1, e1), Tlabeled (t2, e2) ->
		unifyTE1 (Tnode t1) rootCS;
		unifyTE1 (Tnode t2) rootCS;
		unifyE1  e1 rootCS;
		unifyE1  e2 rootCS;
		let typeSubs = unifyTwoTE1 t1.texp_node t2.texp_node rootCS in
		(*let _ = applyAndClean typeSubs rootCS in
		let labSubs = unifyTwoE1 e1 e2 rootCS in
		labSubs*)
		typeSubs (* fixme: in complete implementation this line should be deleted and the three lines above should be uncommented *)
	| Tcon (Tint, []), Tcon (Tint, []) ->
		(Tvar (-1), Tvar (-1));
	| Tcon (Tbool, []), Tcon (Tbool, []) ->
		(Tvar (-1), Tvar (-1));
	| Tcon (Tlab _, []), Tcon (Tlab _, []) ->
		(* fixme: expression unification *)
		(Tvar (-1), Tvar (-1));
	| _ ->
		raise (UnunifyingConstraints ((string_of_short_texp (texp te1))^" != "^(string_of_short_texp (texp te2)))); (* fixme: Error should be implemented *)
and
unifyTE1 currentTE rootCS =
	match currentTE with
		  Tempty ->
			-3
		| Tnode node ->
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
					unifyTE1 currentTE rootCS;
			end
			
;;
(* ==================================================== Unify Types ================================================== *)


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


