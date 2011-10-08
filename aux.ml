open InferDefs;;
open ConstraintUnify;;
open PrettyPrinter;;

(*|++++++++++++++++++++++++++++++++++++++ Clone +++++++++++++++++++++++++++++++++++++++*)
let rec cloneEDesc ed =
	match ed with
	| Evar v ->
			Evar v
	| Econ expr ->
			Econ expr
(* fixme *)
and

cloneEExp parent e =
	let result = { eexp_node = Evar (-1); elink_node = Eempty; elink_parent = Eempty; emark = 0 } in
	let node = cloneEDesc e.eexp_node in
	let link =
		begin match e.elink_node with
			| Eempty ->
					Eempty
			| Enode next ->
					Enode (cloneEExp (Enode result) next)
		end in
	let mark = e.emark in
	result.eexp_node <- node;
	result.elink_node <- link;
	result.elink_parent <- parent;
	result.emark <- mark;
	result
;;

exception NotTDesc of string;;

let rec cloneTDesc td =
	match td with
	| Tvar v ->
			Tvar v
	| Tcon (Tarrow, [t1; t2]) ->
			Tcon (Tarrow, [(cloneTExp Tempty t1); (cloneTExp Tempty t2)])
	| Tcon (Tarrow, _) ->
			raise (NotTDesc "Incorrect Arrow")
	| Tcon (Tint, i) ->
			Tcon (Tint, i)
	| Tcon (Tbool, b) ->
			Tcon (Tbool, b)
	| Tcon (Tlab e, t) ->
			Tcon (Tlab e, t)
	| Tlabeled (t, el) ->
			Tlabeled ((cloneTExp Tempty t), (List.map (cloneEExp Eempty) el))

and

(**
In initail call, parent should be Tempty
*)
cloneTExp parent t =
	let result = { texp_node = Tvar (-1); tlink_node = Tempty; tlink_parent = Tempty; tmark = 0 } in
	let node = cloneTDesc t.texp_node in
	let link =
		begin match t.tlink_node with
			| Tempty ->
					Tempty
			| Tnode next ->
					Tnode (cloneTExp (Tnode result) next)
		end in
	let mark = t.tmark in
	result.texp_node <- node;
	result.tlink_node <- link;
	result.tlink_parent <- parent;
	result.tmark <- mark;
	result
;;

(*|let delTExpNode csNode te =                        *)
(*|	begin match te.tlink_node with                   *)
(*|		| Tnode next ->                                *)
(*|				begin match te.tlink_parent with           *)
(*|					| Tnode teParent ->                      *)
(*|							teParent.tlink_node <- te.tlink_node;*)
(*|							next                                 *)
(*|					| Tempty ->                              *)
(*|							next.tlink_parent <- Tempty;         *)
(*|							csNode.cnstrnt <- next;              *)
(*|							next                                 *)
(*|				end                                        *)
(*|		| Tempty ->                                    *)
(*|				begin match te.tlink_parent with           *)
(*|					| Tnode teParent ->                      *)
(*|							teParent.tlink_node <- Tempty;       *)
(*|							teParent                             *)
(*|					| Tempty ->                              *)
(*|							te                                   *)
(*|				end                                        *)
(*|	end                                              *)
(*|;;                                                 *)

let rec trimClonedTExp te =
	let currTrimmed, link =
		begin match te.texp_node with
			| Tcon (Tarrow, [t1; t2]) ->
					let tt1 = trimClonedTExp t1 in
					let tt2 = trimClonedTExp t2 in
					tt1.tlink_parent <- Tempty;
					tt2.tlink_parent <- Tempty;
					Tnode (teyxp (Tcon (Tarrow, [tt1; tt2]))), te.tlink_node
			| Tlabeled (t, e) ->
					if (getTStat t.texp_node globalGTnode) then
						Tnode te, te.tlink_node
					else
						begin match t.texp_node with
							| Tvar var ->
									begin match te.tlink_node with
										| Tempty ->
												(Tnode te), te.tlink_node
										| Tnode nextNode ->
												let tte = trimClonedTExp nextNode in
												Tnode tte, nextNode.tlink_node
									end
							| _ ->
									let tt = trimClonedTExp t in
									tt.tlink_parent <- Tempty;
									Tnode (teyxp (Tlabeled (tt, e))), te.tlink_node
						end
			| _ ->
					Tnode te, te.tlink_node
		end in
	let linkTrimmed =
		match link with
		| Tempty -> Tempty
		| Tnode node ->
				Tnode (trimClonedTExp node)
	in
	begin match currTrimmed with
		| Tempty ->
				assert false
		| Tnode node ->
				node.tlink_node <- linkTrimmed;
				begin match linkTrimmed with
					| Tempty ->
							node
					| Tnode linkNode ->
							linkNode.tlink_parent <- currTrimmed;
							node
				end
	end
;;

let rec cloneCS parent cs =
	let result = { cnstrnt = teyxp (Tvar (-1)); link = Cempty } in
	let cnstrnt = cloneTExp Tempty cs.cnstrnt in
	let link =
		begin match cs.link with
			| Cempty ->
					Cempty
			| Cnode next ->
					Cnode (cloneCS (Cnode result) next)
		end in
	result.cnstrnt <- cnstrnt;
	result.link <- link;
	result
;;
(*|====================================== Clone =======================================*)

(*|++++++++++++++++++++++++++++++++++++++ Equal +++++++++++++++++++++++++++++++++++++++*)
let areExpEqual e1 e2 = true;; (* fixme *)

let areTvarsEqual (Tvar tv1) (Tvar tv2) =
	if (tv1 == tv2) then
		true
	else
		false
;;

let rec areTarrowsEqual (Tcon (Tarrow, [arg11; arg12])) (Tcon (Tarrow, [arg21; arg22])) =
	let firstArg = areTexpEqual arg11.texp_node arg21.texp_node in
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

let rec canFindEquality tv1 tv2 subs =
	if (List.length subs) == 0 then
		false
	else
		let st1, st2 = List.hd subs in
		if (areTexpEqual st1 tv1) then
			if (areTexpEqual st2 tv2) then
				true
			else
				canFindEquality st2 tv2 (List.tl subs)
		else
			canFindEquality tv1 tv2 (List.tl subs)
;;
(*|====================================== Equal =======================================*)

let rec applySubsOnGVL subs newGvl resultList =
	let length = (List.length newGvl) in
	if (length == 0) then
		resultList
	else
		begin
			let st1, st2 = subs in
			let hd = (List.hd newGvl) in
			if (areTexpEqual st1 hd) then
				applySubsOnGVL subs (List.tl newGvl) (resultList @ [st2])
			else
				applySubsOnGVL subs (List.tl newGvl) (resultList @ [hd])
		end
;;

let rec applySubsListOnGVL subs newGvl resultList =
	let length = (List.length subs) in
	if (length == 0) then
		resultList
	else
		let resultList = applySubsOnGVL (List.hd subs) newGvl [] in
		applySubsListOnGVL (List.tl subs) resultList resultList
;;

let rec getMember i counter list =
	if (i == !counter) then
		List.hd list
	else
		getMember i (incr counter; counter) (List.tl list)
;;

let changeGStatusInTVar newStat t =
	match t.texp_node with
	| Tvar v ->
			setTStat v newStat globalGTnode GTempty
	| _ ->
			false
;;

let changeGStatusInTE newStat te =
	match te.texp_node with
	| Tlabeled (t, e) ->
			tTraverseDownLinks_and_Apply (changeGStatusInTVar newStat) t;
			true
	| _ ->
			false
;;

let rec findBoundVarsInVLs gvl newGvl resultList =
	let length = List.length gvl in
	if (length == 0) then
		resultList
	else
		begin
			let gvh = (List.hd gvl) in
			let ngvh = (List.hd newGvl) in
			begin match ngvh with
				| Tvar v ->
						findBoundVarsInVLs (List.tl gvl) (List.tl newGvl) resultList
				| _ ->
						findBoundVarsInVLs (List.tl gvl) (List.tl newGvl) (resultList@[gvh])
			end;
		end
;;

let findBoundVarsInTE csRoot =
	let csClone = cloneCS Cempty csRoot in
	let subs = unifyCS1 csClone csClone in
	if (List.length subs) != 0 then
		begin
			let gvl = listAllVars (getLastTVar()) [] (ref 0) in
			let makeTVar v = Tvar v in
			let newGvl = (List.map makeTVar gvl) in
			let newGvl = applySubsListOnGVL subs newGvl [] in
			let freeVars = findBoundVarsInVLs gvl newGvl [] in
			freeVars
		end
	else
		[]
;;

let makeTEVar v = teyxp(Tvar v)
let changeGStatusBoundVarsInTVar boundVarList newStat =
	let boundTvarList = List.map makeTEVar boundVarList in
	List.map (changeGStatusInTVar newStat) boundTvarList
;;

(*|let tPropagateMark t marker =                             *)
(*|	let _ = tTraverseDownLinks_and_Apply (tmark marker) t in*)
(*|	tTraverseUpLinks_and_Apply (tmark marker) t;;           *)

let tLink t1 t2 rootCS =
	(*|	let t1 =                                  *)
	(*|		typeInstance Tempty t1;                 *)
	(*|(*|		if (t1.tmark == (gable + ged)) then*)*)
	(*|(*|			typeInstance Tempty t1           *)*)
	(*|(*|		else                               *)*)
	(*|(*|			t1                               *)*)
	(*|	in                                        *)
	(*|	let t2 =                                  *)
	(*|			typeInstance Tempty t2                *)
	(*|(*|		if (t2.tmark == (gable + ged)) then*)*)
	(*|(*|			typeInstance Tempty t2           *)*)
	(*|(*|		else                               *)*)
	(*|(*|			t2                               *)*)
	(*|	in                                        *)
	
	(*|	if (mark == (ngable + nged)) then*)
	
	(*|	if (not generalizable) then                                             *)
	(*|		begin                                                                 *)
	(*|			let _ = tTraverseDownLinks_and_Apply (changeGStatusInTE false) t1 in*)
	(*|			let _ = tTraverseUpLinks_and_Apply (changeGStatusInTE false) t1 in  *)
	(*|			let _ = tTraverseDownLinks_and_Apply (changeGStatusInTE false) t2 in*)
	(*|			let _ = tTraverseUpLinks_and_Apply (changeGStatusInTE false) t2 in  *)
	(*|			()                                                                  *)
	(*|		end                                                                   *)
	(*|	;                                                                       *)
	
	(*|	if t1.tmark != mark then                         *)
	(*|		begin let _ = tPropagateMark t1 mark in () end;*)
	(*|	if t2.tmark != mark then                         *)
	(*|		begin let _ = tPropagateMark t2 mark in () end;*)
	let lastNode = trepr (Tnode t1) in
	t2.tlink_parent <- (Tnode lastNode);
	lastNode.tlink_node <- (Tnode t2);
	addC t1 rootCS;
	let boundVars = findBoundVarsInTE rootCS in
	changeGStatusBoundVarsInTVar boundVars false;
;;

(*|let eLink e1 e2 mark =                *)
(*|	let lastNode = erepr (Enode e1) in  *)
(*|	(*|	ePropagateMark e2 mark;*)      *)
(*|	e2.elink_parent <- (Enode lastNode);*)
(*|	lastNode.elink_node <- (Enode e2);; *)

(*|(*|+++++++++++++++++++++++ Extract Generalized Variables In CS ++++++++++++++++++++++++*)*)
(*|let rec varInList v gvl =                                                                *)
(*|	let length = List.length gvl in                                                        *)
(*|	if (length == 0) then                                                                  *)
(*|		false                                                                                *)
(*|	else                                                                                   *)
(*|	if (List.hd gvl) == v then                                                             *)
(*|		true                                                                                 *)
(*|	else                                                                                   *)
(*|		varInList v (List.tl gvl)                                                            *)
(*|;;                                                                                       *)
(*|                                                                                         *)
(*|let isInGeneralizedVars gvl te =                                                         *)
(*|	match te.texp_node with                                                                *)
(*|	| Tvar v ->                                                                            *)
(*|			if (varInList v gvl) then                                                          *)
(*|				true                                                                             *)
(*|			else                                                                               *)
(*|				false                                                                            *)
(*|	| _ ->                                                                                 *)
(*|			false                                                                              *)
(*|;;                                                                                       *)
(*|                                                                                         *)
(*|let rec extractGeneralizedVarsRelationsTE gvl te =                                       *)
(*|	if (isInGeneralizedVars gvl te) then                                                   *)
(*|		begin match te.tlink_node with                                                       *)
(*|			| Tnode next ->                                                                    *)
(*|					extractGeneralizedVarsRelationsTE gvl next                                     *)
(*|			| Tempty ->                                                                        *)
(*|					begin match te.tlink_parent with                                               *)
(*|						| Tnode parent ->                                                            *)
(*|								false                                                                    *)
(*|						| Tempty ->                                                                  *)
(*|								true                                                                     *)
(*|					end                                                                            *)
(*|		end                                                                                  *)
(*|	else                                                                                   *)
(*|		begin match te.tlink_node with                                                       *)
(*|			| Tnode next ->                                                                    *)
(*|					next.tlink_parent <- te.tlink_parent;                                          *)
(*|					extractGeneralizedVarsRelationsTE gvl next                                     *)
(*|			| Tempty ->                                                                        *)
(*|					begin match te.tlink_parent with                                               *)
(*|						| Tnode parent ->                                                            *)
(*|								parent.tlink_node <- Tempty;                                             *)
(*|								begin match parent.tlink_parent with                                     *)
(*|									| Tnode parentOfparent ->                                              *)
(*|											false                                                              *)
(*|									| Tempty ->                                                            *)
(*|											true                                                               *)
(*|								end                                                                      *)
(*|						| Tempty ->                                                                  *)
(*|								true                                                                     *)
(*|					end                                                                            *)
(*|		end                                                                                  *)
(*|;;                                                                                       *)
(*|                                                                                         *)
(*|let rec extractGeneralizedVarsRelationsCS gvl currCS parentCS csCloneRoot =              *)
(*|	let csNodeRemoval = extractGeneralizedVarsRelationsTE gvl currCS.cnstrnt in            *)
(*|	if csNodeRemoval then                                                                  *)
(*|		begin match currCS.link with                                                         *)
(*|			| Cnode newNext ->                                                                 *)
(*|					begin match parentCS with                                                      *)
(*|						| Cnode csParent ->                                                          *)
(*|								csParent.link <- currCS.link;                                            *)
(*|						| Cempty ->                                                                  *)
(*|								csCloneRoot.link <- currCS.link;                                         *)
(*|					end;                                                                           *)
(*|					extractGeneralizedVarsRelationsCS gvl newNext parentCS csCloneRoot;            *)
(*|			| Cempty ->                                                                        *)
(*|					begin match parentCS with                                                      *)
(*|						| Cnode csParent ->                                                          *)
(*|								csParent.link <- currCS.link;                                            *)
(*|								csCloneRoot                                                              *)
(*|						| Cempty ->                                                                  *)
(*|								csInit()                                                                 *)
(*|					end                                                                            *)
(*|		end                                                                                  *)
(*|	else                                                                                   *)
(*|		begin match currCS.link with                                                         *)
(*|			| Cnode newNext ->                                                                 *)
(*|					extractGeneralizedVarsRelationsCS gvl newNext (Cnode currCS) csCloneRoot       *)
(*|			| Cempty ->                                                                        *)
(*|					csCloneRoot                                                                    *)
(*|		end                                                                                  *)
(*|;;                                                                                       *)

let findRelations gvl newGvl cs =
	let i = ref (-1) in
	while !i != ((List.length newGvl) -1) do
		incr i;
		let j = ref (!i + 1) in
		while !j != (List.length newGvl) do
			let gvc1 = getMember !i (ref 0) newGvl in
			let gvc2 = getMember !j (ref 0) newGvl in
			if (areTexpEqual gvc1 gvc2) then
				begin
					let gv1 = getMember !i (ref 0) gvl in
					let gv2 = getMember !j (ref 0) gvl in
					let t1 = teyxp (Tvar gv1) in
					let t2 = teyxp (Tvar gv2) in
					(*|					tLink t1 t2 (gable + ged);*)
					(*|	**************************** FIXME: is it ok to pass cs to tLink ************************************* *)
					tLink t1 t2 cs;
					addC t1 cs;
				end;
			incr j;
		done;
	done;
	cs
;;

let rec extractGeneralizedVarsRelationsCS gvl csClone =
	let subs = unifyCS1 csClone csClone in
	let makeVar v = Tvar v in
	let newGvl = (List.map makeVar gvl) in
	let newGvl = applySubsListOnGVL subs newGvl [] in
	let csClone = findRelations gvl newGvl (csInit()) in
	csClone
;;
(*|======================= Extract Generalized Variables In CS ========================*)

(*|+++++++++++++++++++++++++++++++++++ Generalize +++++++++++++++++++++++++++++++++++++*)
let generalizeElist resList e =
	let resE =
		if (getEStat e.eexp_node globalGEnode) then
			emark (ged) e
		else
			e
	in
	resList@[resE]
;;
let rec generalizeSingle te =
	(*|	if t.tmark == (gable + nged) then*)
	(*|		tmark (gable + ged) t          *)
	(*|	else                             *)
	(*|		t                              *)
	match te.texp_node with
	| Tvar v ->
			if (getTStat te.texp_node globalGTnode) then
				tmark (ged) te
			(*|				tmark (gable + ged) t*)
			else
				te
	| Tlabeled (t, el) ->
			let newEl = List.fold_left generalizeElist [] el in
			te.texp_node <- Tlabeled (t, newEl);
			te
	| _ ->
			te
;;

let generalize t =
	let _ = tTraverseDownAll_and_Apply generalizeSingle t in
	let _ = tTraverseUpAll_and_Apply generalizeSingle t in
	t
;;

let findGensInEList resList e =
	match e.eexp_node with
	| Evar v ->
			if e.emark == (ged) then
				resList@[v]
			else
				resList
	| _ ->
			resList
;;

let listGeneralizedEVars te =
	match te.texp_node with
	| Tlabeled (t, el) ->
			List.fold_left findGensInEList [] el
	| _ ->
			[]
;;
let listGeneralizedVars te =
	match te.texp_node with
	| Tvar v ->
			if te.tmark == (ged) then
				v
			else
				-5
	| _ ->
	-5
(*|	let res =                            *)
(*|		begin                              *)
(*|			if te.tmark == (gable + ged) then*)
(*|				begin match te.texp_node with  *)
(*|					| Tvar v ->                  *)
(*|							v                        *)
(*|					| Tlabeled (t, el) ->        *)
(*|					(* fixme *)                  *)
(*|					-5                           *)
(*|					| _ ->                       *)
(*|					-5                           *)
(*|				end                            *)
(*|			else                             *)
(*|				-5                             *)
(*|		end in                             *)
(*|	res                                  *)
;;
(*|=================================== Generalize =====================================*)

(*|++++++++++++++++++++++++++++++++++ Instantiate +++++++++++++++++++++++++++++++++++++*)
let rec instanciateFreeVars_InTlabeled labelList resultList =
	let length = (List.length labelList) in
	if (length == 0) then
		resultList
	else
		begin
			let hd = (List.hd labelList) in
			begin match hd.eexp_node with
					Evar v ->
						let newVar = (evar()).eexp_node in
						instanciateFreeVars_InTlabeled (List.tl labelList) (resultList @ [emark (nged) (eeyxp newVar)])
				(*|						instanciateFreeVars_InTlabeled (List.tl labelList) (resultList @ [emark (gable + nged) (eeyxp newVar)])*)
				| _ ->
						instanciateFreeVars_InTlabeled (List.tl labelList) (resultList @ [hd])
			end
		end
;;

let subsVarVar_InSingleTE v1 v2 te =
	match te.texp_node with
	| Tvar v ->
			if (v == v1) then
				begin
					te.texp_node <- v2;
					(*|					let _ = tmark (gable + nged) te in*)
					let _ = tmark (nged) te in
					()
				end;
			()
	
	| _ ->
			()
;;

let subsEInEL v1 v2 resList e =
	let res =
		begin match e.eexp_node with
			| Evar v ->
					if (v == v1) then
						begin
							let newE = eeyxp (Evar v2) in
							let _ = emark (nged) newE in
							newE;
						end
					else
						e
			| _ ->
					eeyxp (Evar (-5))
		end in
	resList@[res]
;;

let subsEVarEVar_InSingleTE v1 v2 te =
	match te.texp_node with
	| Tlabeled (t, el) ->
			let newEL = List.fold_left (subsEInEL v1 v2) [] el in
			te.texp_node <- Tlabeled (t, el)
	| _ ->
			()
;;

let instanciateFreeVars teRootOfClone te =
	begin match te.texp_node with
		| Tvar v ->
				if te.tmark == (ged) then
					(*|				if (getTStat te.texp_node globalGTnode) then*)
					begin
						let Tvar newVar = (tvar()).texp_node in
						let _ = tTraverseDownAll_and_Apply (subsVarVar_InSingleTE v (Tvar newVar)) teRootOfClone in
						let _ = tTraverseUpAll_and_Apply (subsVarVar_InSingleTE v (Tvar newVar)) teRootOfClone in
						newVar
					end
				else
					-5
		| Tlabeled (t, el) ->
		(* fixme: change it to match "Tvar v"'s behaviour above *)
		(*|					let resultantLabels = instanciateFreeVars_InTlabeled el [] in*)
		(*|					let _ = te.texp_node <- Tlabeled (t, resultantLabels) in     *)
		(*|					let _ = tmark (gable + nged) te in                           *)
		-5
		| _ ->
		-5
	end;
(*|	if te.tmark == (gable + ged) then                                                                  *)
(*|		begin match te.texp_node with                                                                    *)
(*|			| Tvar v ->                                                                                    *)
(*|					let Tvar newVar = (tvar()).texp_node in                                                    *)
(*|					let _ = tTraverseDownAll_and_Apply (subsVarVar_InSingleTE v (Tvar newVar)) teRootOfClone in*)
(*|					let _ = tTraverseUpAll_and_Apply (subsVarVar_InSingleTE v (Tvar newVar)) teRootOfClone in  *)
(*|					newVar                                                                                     *)
(*|			| Tlabeled (t, el) ->                                                                          *)
(*|			(* fixme: change it to match "Tvar v"'s behaviour above *)                                     *)
(*|					let resultantLabels = instanciateFreeVars_InTlabeled el [] in                              *)
(*|					let _ = te.texp_node <- Tlabeled (t, resultantLabels) in                                   *)
(*|					let _ = tmark (gable + nged) te in                                                         *)
(*|					-3                                                                                         *)
(*|			| _ ->                                                                                         *)
(*|					print_string "Wrong!!!!!!!!";                                                              *)
(*|					-4                                                                                         *)
(*|		end                                                                                              *)
(*|	else                                                                                               *)
(*|		-5                                                                                               *)
;;

let findAndInsEs teRootOfClone resList e =
	let res =
		begin match e.eexp_node with
			| Evar v ->
					if e.emark == (ged) then
						begin
							let Evar newVar = (evar()).eexp_node in
							let _ = tTraverseDownAll_and_Apply (subsEVarEVar_InSingleTE v newVar) teRootOfClone in
							let _ = tTraverseUpAll_and_Apply (subsEVarEVar_InSingleTE v newVar) teRootOfClone in
							v, newVar
						end
					else
						-6, v
			| _ ->
			-5, -5
		end in
	resList@[res]
;;

let instanciateFreeEVars teRootOfClone te =
	begin match te.texp_node with
		| Tlabeled (t, el) ->
				let newPairList = List.fold_left (findAndInsEs teRootOfClone) [] el in
				let gvl, newGvl = List.split newPairList in
				gvl, newGvl
		| _ ->
				[-5], [-5]
	end;
;;

let rec gvExists gv gvl =
	let length = (List.length gvl) in
	if (length == 0) then
		false
	else
		begin
			if (List.hd gvl) == gv then
				true
			else
				gvExists gv (List.tl gvl)
		end
;;

let cleanGVL gvlResult gv =
	if gv == (-5) then
		gvlResult
	else
		begin
			if gvExists gv gvlResult then
				gvlResult
			else
				gvlResult @ [gv]
		end
;;

let cleanEGVL egvlResult egv =
	if (egv == []) || ((List.hd egv) == (-5)) || ((List.hd egv) == (-6)) then
		egvlResult
	else
		egvlResult @ egv
;;

let typeInstance t iNodeRoot ieNodeRoot =
	let clone = cloneTExp Tempty t in
	(*|	let csNode = (csInit()) in        *)
	(*|	let _ = csNode.cnstrnt <- clone in*)
	(*|	let clone = tTraverseDownAll_and_Apply (trimClonedTExp csNode) clone; csNode.cnstrnt in*)
	let clone = trimClonedTExp clone in
(*|	let egvl = tTraverseDownAll_and_Apply listGeneralizedEVars clone in*)
(*|	let egvl = List.fold_left cleanEGVL [] egvl in                     *)
	let gvl = tTraverseDownAll_and_Apply (listGeneralizedVars) clone in
	let gvl = List.fold_left cleanGVL [] gvl in
	if (List.length gvl) == 0 then
		(*|		let _ = tTraverseDownAll_and_Apply (instanciateFreeVars (csInit()) clone) clone in*)
		(*|		let _ = tTraverseDownAll_and_Apply (instanciateFreeVars clone) clone in*)
		clone
	else
		(*|		let csClone = cloneCS Cempty csRoot in                                         *)
		(*|		let resCS = extractGeneralizedVarsRelationsCS gvl csClone in                   *)
		(*|		let csClone = resCS in                                                         *)
		(*|		let _ = tTraverseDownAll_and_Apply (instanciateFreeVars csClone clone) clone in*)
		let newGvl = tTraverseDownAll_and_Apply (instanciateFreeVars clone) clone in
		let newGvl = List.fold_left cleanGVL [] newGvl in
		let egvl, newEGvl = List.split (tTraverseDownAll_and_Apply (instanciateFreeEVars clone) clone) in
		let egvl = List.fold_left cleanEGVL [] egvl in
		let newEGvl = List.fold_left cleanEGVL [] newEGvl in
		(*|		(crepr (Cnode csRoot)).link <- (Cnode csClone);*)
		(irepr iNodeRoot).ilink <- Inode (iNode (gvl, newGvl));
		(ierepr ieNodeRoot).ielink <- IEnode (ieNode (egvl, newEGvl));
		clone
;;
(*|================================== Instantiate =====================================*)
let forAllgvs gvl newGvl cs =
	let i = ref (-1) in
	let j = ref (-1) in
	while !i != ((List.length newGvl) -1) do
		incr i;
		incr j;
		let gv = getMember !i (ref 0) gvl in
		let gvc = getMember !j (ref 0) newGvl in
		let _ = applyOnAllTEs (subsVarVar_InSingleTE gv (Tvar gvc)) cs in
		()
	done;
	cs
;;

let createExtraRelationsForEachINode iNode csRoot =
	let gvl, newGvl = iNode in
	let csClone = cloneCS Cempty csRoot in
	let resCS = extractGeneralizedVarsRelationsCS gvl csClone in
	let csClone = resCS in
	let _ = forAllgvs gvl newGvl csClone in
	(crepr (Cnode csRoot)).link <- (Cnode csClone);
;;

let rec createExtraRelations iNodeRoot csRoot =
	match iNodeRoot with
	| Iempty ->
			csRoot
	| Inode node ->
			createExtraRelationsForEachINode node.pairs csRoot;
			createExtraRelations node.ilink csRoot
;;

let createExtraERelationsForEachINode ieNode csRoot =
	let egvl, newEGvl = ieNode in
	let csClone = cloneCS Cempty csRoot in
	let resCS = extractGeneralizedVarsRelationsCS egvl csClone in
	let csClone = resCS in
	let _ = forAllgvs egvl newEGvl csClone in
	(crepr (Cnode csRoot)).link <- (Cnode csClone);
;;

let rec createExtraERelations ieNodeRoot csRoot =
	match ieNodeRoot with
	| IEempty ->
			csRoot
	| IEnode node ->
			createExtraERelationsForEachINode node.epairs csRoot;
			createExtraERelations node.ielink csRoot
;;

