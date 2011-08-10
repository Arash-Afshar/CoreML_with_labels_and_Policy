open InferDefs;;
open PrettyPrinter;;

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
		| Tlabled (t, e) ->
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
	| Tlabled (t, e) ->
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
	| Tlabled (t, e) ->
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
	| Tlabled (t, e) ->
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
	| Tlabled (t1, e1), Tlabled (t2, e2) ->
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
		| Tlabled (t, e) ->
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
*)



(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++ Subs Var with Var ++++++++++++++++++++++++++++++++++++++++++++++++ *)
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
	| Tlabled (t, e) ->
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
	| Tlabled (t, e) ->
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


let rec unifyTwoE e1 e2 rootCS subs = [];;

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++ Unify +++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec unifyTwoTE te1 te2 rootCS subs =
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
	| Tcon (Tlab, []), Tcon (Tlab, []) ->
		subs;
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->
		let argUnify = unifyTwoTE t11.texp_node t21.texp_node rootCS subs in
		(* let unifiedT12 = applySubs (List.hd argUnify) rootCS  *) (* fixme: the line should correctly be implemented *)
		let resUnify = unifyTwoTE t21.texp_node t22.texp_node rootCS subs in
		argUnify@resUnify@subs;
	| Tlabled (t1, e1), Tlabled (t2, e2) ->
		let typeUnify = unifyTwoTE t1.texp_node t2.texp_node rootCS subs in
		(* let unifiedT12 = applySubs (List.hd argUnify) rootCS  *) (* fixme: the line should correctly be implemented *)
		let labUnify = unifyTwoE e1 e2 rootCS subs in
		typeUnify@labUnify@subs;
	| _ ->
		subs; (* fixme: Error should be implemented *)
;;

let rec unifyTE currentTE rootCS subs =
	let subs =
	begin match currentTE.texp_node with
		  Tvar var ->
			subs
		| Tcon (sym, l)->
			begin match sym with
				  Tarrow ->
					let subs = unifyTE (List.hd l) rootCS subs in
					let subs = unifyTE (List.hd (List.tl l)) rootCS subs in
					subs
				| _ ->
					subs
			end;
		| Tlabled (t, e) ->
			let subs = unifyTE t rootCS subs in
			subs
		| _ -> subs
	end
	in 
	begin match currentTE.tlink_node with
		  Tempty -> subs
		| Tnode node ->
			let subs = unifyTwoTE (currentTE.texp_node) (node.texp_node) rootCS subs in
			 subs
	end
;;

let rec unifyCS currentC rootCS subs =
	let subs = unifyTE currentC.cnstrnt rootCS subs in
	match currentC.link with
	  Cnode node ->
		let subs = unifyCS node rootCS subs in
		subs
	| Cempty -> subs
;;
(* ======================================================= Unify ===================================================== *)


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
		| Tcon (Tlab, _), Tcon (Tlab, _) -> true (* fixme *)
		| Tlabled (t1, e1), Tlabled (t2, e2) ->
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


let rec delEqCS csNode =
	delEqTE (Tnode csNode.cnstrnt);
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
		| Tlabled (t, e) ->
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



(* ================================================== Delete Redundant =============================================== *)


(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++ apply substitute +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec applySubs (te1, te2) csNode =
	match te1, te2 with
	  Tvar var1, Tvar var2 ->
		subsVarVarCS (var1, var2) csNode; -45
	| Tvar var, other ->
		subsVarOtherCS (var, other) csNode; -46
	| other, Tvar var ->
		subsVarOtherCS (var, other) csNode; -47
	| Tcon (Tint, []), Tcon (Tint, []) ->
		-48
	| Tcon (Tbool, []), Tcon (Tbool, []) ->
		-49
	| Tcon (Tlab, []), Tcon (Tlab, []) ->
		-50
	| Tcon (Tarrow, [t11; t12]), Tcon (Tarrow, [t21; t22]) ->
		-51 (* fixme: error *)
	| Tlabled (t1, e1), Tlabled (t2, e2) ->
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


