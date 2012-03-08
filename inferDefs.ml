open Syntax;;

type eeeenode = { mutable eexp_node : edesc; mutable elink_node : eexp; mutable elink_parent: eexp; mutable emark : int }
and eexp = Eempty | Enode of eeeenode
and edesc = Evar of int | Econ of expr;;

type type_symbol = Tarrow | Tint | Tbool | Tlab of eeeenode
type ttttnode = { mutable texp_node : tdesc; mutable tlink_node : texp; mutable tlink_parent: texp; mutable tmark : int }
and texp = Tempty | Tnode of ttttnode
and tdesc = Tvar of int | Tcon of type_symbol * ttttnode list | Tlabeled of ttttnode * eeeenode list;;

let eeyxp e = { eexp_node = e; elink_node = Eempty; elink_parent = Eempty; emark = 0 };;
let teyxp d = { texp_node = d; tlink_node = Tempty; tlink_parent = Tempty; tmark = 0 };;

type expTypePairNode = { mutable etExp : expr; mutable etType: ttttnode; mutable etLink: expTypePair }
and expTypePair = ETempty | ETnode of expTypePairNode;;
(* let expTypePair e t = {etExp = e; etType = t; etLink = ETempty};; let   *)
(* initExpTypePair = {etExp = Var "NAN"; etType = texp (Tvar (-1)); etLink *)
(* = ETempty};;                                                            *)

(* type scheme = texp list * texp;; *)

type generalizableTStatus = (int * bool)
and gTStatusVector = { mutable tstat : generalizableTStatus; mutable gtlink : gTNode }
and gTNode = GTempty | GTnode of gTStatusVector;;
let gTNode g = { tstat = g ; gtlink = GTempty }
let gTInit() = { tstat = (-1, false) ; gtlink = GTempty };;
let globalGTnode = GTnode (gTInit());;
let rec gtrepr g =
	match g with
		GTnode gn ->
			begin match gn.gtlink with
					GTnode u -> let v = gtrepr (GTnode u) in v
				| GTempty -> gn
			end
	| GTempty -> assert false;;

let addTStat var stat node =
	(gtrepr node).gtlink <- GTnode (gTNode (var, stat))
;;

let rec getTStat te gTNode =
	match te with
	| Tvar var ->
			begin match gTNode with
				| GTempty ->
				(* error *)
						false
				| GTnode node ->
						let v, b = node.tstat in
						if (var == v) then
							b
						else
							getTStat te node.gtlink
			end
	| Tlabeled (t, e) ->
			begin match t.texp_node with
				| Tvar v ->
						getTStat t.texp_node gTNode
				| _ ->
						false
			end
	| _ ->
			false
;;

let rec setTStat var newStat currNode parentNode =
	match currNode with
	| GTempty ->
	(* error *)
			false
	| GTnode node ->
			let v, b = node.tstat in
			if (var == v) then
				begin match parentNode with
					| GTempty ->
					(* error *)
							false
					| GTnode parent ->
							let newCurrent = gTNode (var, newStat) in
							parent.gtlink <- (GTnode newCurrent);
							newCurrent.gtlink <- node.gtlink;
							true
				end
			else
				begin match node.gtlink with
					| GTempty ->
							false
					| next ->
							setTStat var newStat next currNode
				end
;;

type generalizableEStatus = (int * bool)
and gEStatusVector = { mutable estat : generalizableEStatus; mutable gelink : gENode }
and gENode = GEempty | GEnode of gEStatusVector;;
let gENode g = { estat = g ; gelink = GEempty }
let gEInit() = { estat = (-1, false) ; gelink = GEempty };;
let globalGEnode = GEnode (gEInit());;
let rec gerepr g =
	match g with
		GEnode gn ->
			begin match gn.gelink with
					GEnode u -> let v = gerepr (GEnode u) in v
				| GEempty -> gn
			end
	| GEempty -> assert false;;

let addEStat var stat node =
	(gerepr node).gelink <- GEnode (gENode (var, stat))
;;

let rec getEStat e geNode =
	match e with
	| Evar var ->
			begin match geNode with
				| GEempty ->
				(* error *)
						false
				| GEnode node ->
						let v, b = node.estat in
						if (var == v) then
							b
						else
							getEStat e node.gelink
			end
	| _ ->
			false
;;

let rec setEStat var newStat currNode parentNode =
	match currNode with
	| GEempty ->
	(* error *)
			false
	| GEnode node ->
			let v, b = node.estat in
			if (var == v) then
				begin match parentNode with
					| GEempty ->
					(* error *)
							false
					| GEnode parent ->
							let newCurrent = gENode (var, newStat) in
							parent.gelink <- (GEnode newCurrent);
							newCurrent.gelink <- node.gelink;
							true
				end
			else
				begin match node.gelink with
					| GEempty ->
							false
					| next ->
							setEStat var newStat next currNode
				end
;;

let chooseMark t1 t2 =
	(getTStat t1.texp_node globalGTnode) & (getTStat t2.texp_node globalGTnode)
;;

(*|                     "generalizable=1" "not generalizable=0" *)
(*|                      -----------------------------------    *)
(*|"generalized=+4"     |        5        |     4=error     |   *)
(*|                     |-----------------------------------|   *)
(*|"not generalized=+2" |        3        |        2        |   *)
(*|                      -----------------------------------    *)

(* let gable = 1 let ngable = 0 *)
let ged = 4
let nged = 2;;

(*|++++++++++++++++++++++++++++++++++++++ Marks +++++++++++++++++++++++++++++++++++++++*)
let tmark m t = t.tmark <- m; t;;
let emark m e = e.emark <- m; e;;
(*|====================================== Marks =======================================*)

let count = ref 0
let evar() =
	incr count;
	addEStat !count true globalGEnode;
	emark (nged) (eeyxp (Evar !count))
let getLastEVar() = !count;;

let count = ref 0
let tvar() =
	incr count;
	addTStat !count true globalGTnode;
	tmark (nged) (teyxp (Tvar !count))
let getLastTVar() = !count;;

let rec listAllVars lastVar resList counter =
	if (!counter) <= lastVar then
		begin
			let cnt = !counter in
			incr counter;
			listAllVars lastVar (resList@[cnt]) counter;
		end
	else
		resList
;;

(*|let tint = tmark (ngable + nged) (teyxp (Tcon (Tint, [])));;*)
let tint = tmark (nged) (teyxp (Tcon (Tint, [])));;
let tbool = tmark (nged) (teyxp (Tcon (Tbool, [])));;
let tlab e = tmark (nged) (teyxp (Tcon (Tlab e, [])));;
let tarrow t1 t2 = tmark (nged) (teyxp (Tcon (Tarrow, [t1; t2])));;
let nol = emark (nged) (eeyxp (Econ noLab));;
let tlabeled te e =
	let resT =
		match te.texp_node with
			Tlabeled (t, el) ->
				teyxp (Tlabeled (t, el@[e]))
		| _ ->
				teyxp (Tlabeled (te, [e]))
	(*|	in (tmark (ngable + nged) t)*)
	(*|	in (tmark (nged) resT)*)
	in resT
;;

let tevar() = tlabeled (tvar()) (evar());;

type csNode = { mutable cnstrnt : ttttnode; mutable link : cSet }
and cSet = Cempty | Cnode of csNode;;
let csnode c = { cnstrnt = c ; link = Cempty }
let csInit() = { cnstrnt = tmark (nged) (teyxp (Tvar (-1))) ; link = Cempty };;
(*|let csInit() = { cnstrnt = tmark (ngable + nged) (teyxp (Tvar (-1))) ; link = Cempty };;*)

let rec crepr cs =
	match cs with
		Cnode cs ->
			begin match cs.link with
				(* u -> let Enode v = erepr u in en.eexp_node <- v.eexp_node;      *)
				(* en.elink_node <- Eempty; Enode v                                *)
					Cnode u -> let v = crepr (Cnode u) in v
				| Cempty -> cs
			end
	| Cempty -> assert false;;
let addC c cs = (crepr (Cnode cs)).link <- Cnode (csnode c);;

let rec erepr e =
	match e with
		Enode en ->
			begin match en.elink_node with
				(* u -> let Enode v = erepr u in en.eexp_node <- v.eexp_node;      *)
				(* en.elinmarkk_node <- Eempty; Enode v                            *)
					Enode u -> let v = erepr (Enode u) in v
				| Eempty -> en
			end
	| Eempty -> assert false;;

(*|let edesc e =              *)
(*|	let e = erepr e in       *)
(*|	match e.elink_node with  *)
(*|		Enode u -> assert false*)
(*|	| Eempty -> e.eexp_node;;*)

let rec trepr t =
	match t with
		Tnode tn ->
			begin match tn.tlink_node with
				(* u -> let Tnode v = repr u in tn.texp_node <- v.texp_node;       *)
				(* tn.link_node <- Tempty; Tnode v                                 *)
					Tnode u -> let v = trepr (Tnode u) in v
				| Tempty -> tn
			end
	| Tempty -> assert false;;

let tdesc t =
	let t = trepr t in
	match t.tlink_node with
		Tnode u -> assert false
	| Tempty -> t.texp_node;;

let extractEdesc eNode =
	eNode.eexp_node
;;

(* let extract_eenode (Enode eExp) = eExp ;; *)
let getAllLabels texp =
	match texp.texp_node with
		Tlabeled(t, e) ->
			e
	| _ ->
			[]
;;

(* let rec getUnlabeledPart t = match t.texp_node with Tlabeled(t2,e) ->   *)
(* getUnlabeledPart t2 | _ -> t.texp_node ;;                               *)

type instEPair = (int list) * (int list)
and instantiationEVector = { mutable epairs : instEPair; mutable ielink : ieNode }
and ieNode = IEempty | IEnode of instantiationEVector;;
let ieNode p = { epairs = p ; ielink = IEempty }
let iEInit() = { epairs = ([],[]) ; ielink = IEempty };;
let rec ierepr i =
	match i with
		IEnode iN ->
			begin match iN.ielink with
					IEnode u -> let v = ierepr (IEnode u) in v
				| IEempty -> iN
			end
	| IEempty -> assert false;;

type instPair = (int list) * (int list)
and instantiationVector = { mutable pairs : instPair; mutable ilink : iNode }
and iNode = Iempty | Inode of instantiationVector;;
let iNode p = { pairs = p ; ilink = Iempty }
let iInit() = { pairs = ([],[]) ; ilink = Iempty };;
let rec irepr i =
	match i with
		Inode iN ->
			begin match iN.ilink with
					Inode u -> let v = irepr (Inode u) in v
				| Iempty -> iN
			end
	| Iempty -> assert false;;

(*|++++++++++++++++++++++++++++++++++ Traverse Types ++++++++++++++++++++++++++++++++++*)
(*down, all*)
let rec tTraverseDownAll_and_Apply func t =
	let res1 = func t in
	let res234 =
		begin match t.texp_node with
			| Tcon (Tarrow, [t1; t2]) ->
					let res2 = tTraverseDownAll_and_Apply func t1 in
					let res3 = tTraverseDownAll_and_Apply func t2 in
					res2 @ res3
			| Tlabeled (t, e) ->
					let res4 = tTraverseDownAll_and_Apply func t in
					res4
			| _ -> []
		end in
	let res5 =
		match t.tlink_node with
		| Tempty -> []
		| Tnode node ->
				let res5 = tTraverseDownAll_and_Apply func node in
				res5
	in res5 @ res234 @ [res1]
;;

(* up, all *)
let rec tTraverseUpAll_and_Apply func t =
	let res1 = func t in
	let res234 =
		begin match t.texp_node with
			| Tcon (Tarrow, [t1; t2]) ->
					let res2 = tTraverseUpAll_and_Apply func t1 in
					let res3 = tTraverseUpAll_and_Apply func t2 in
					res2 @ res3
			| Tlabeled (t, e) ->
					let res4 = tTraverseUpAll_and_Apply func t in
					res4
			| _ -> []
		end in
	let res5 =
		match t.tlink_node with
		| Tempty -> []
		| Tnode node ->
				let res5 = tTraverseUpAll_and_Apply func node in
				res5
	in res5 @ res234 @ [res1]
;;

(* down, links *)
let rec tTraverseDownLinks_and_Apply func t =
	let res1 = func t in
	let res2 =
		match t.tlink_node with
		| Tempty -> []
		| Tnode node ->
				let res2 = tTraverseDownLinks_and_Apply func node in
				res2
	in
	res2@[res1]
;;

(* up, links *)
let rec tTraverseUpLinks_and_Apply func t =
	let res1 = func t in
	let res2 =
		match t.tlink_node with
		| Tempty -> []
		| Tnode node ->
				let res2 = tTraverseUpLinks_and_Apply func node in
				res2
	in
	res2@[res1]
;;

(*|================================== Traverse Types ==================================*)

(*|+++++++++++++++++++++++++++++++ Traverse Expressions +++++++++++++++++++++++++++++++*)
(*|let rec eTraverseDownLinks_and_Apply func e =           *)
(*|	func e;                                               *)
(*|	match e.elink_node with                               *)
(*|	| Eempty -> ()                                        *)
(*|	| Enode node -> eTraverseDownLinks_and_Apply func node*)
(*|;;                                                      *)

(*|let rec eTraverseUpLinks_and_Apply func e =           *)
(*|	func e;                                             *)
(*|	match e.elink_parent with                           *)
(*|	| Eempty -> ()                                      *)
(*|	| Enode node -> eTraverseUpLinks_and_Apply func node*)
(*|;;                                                    *)
(*|=============================== Traverse Expressions ===============================*)

(*|++++++++++++++++++++++++++++++++ Traverse EveryWhere +++++++++++++++++++++++++++++++*)
(*down, all*)
let rec cTraverseDownAll_and_Apply func csNode =
	let res1 = func csNode in
	let res2 =
		match csNode.link with
		| Cempty -> []
		| Cnode node ->
				let res2 = cTraverseDownAll_and_Apply func node in
				res2
	in res2 @ res1
;;

let rec applyOnAllTEs func csRootNode =
	let newFunc csRootNode =
		tTraverseDownLinks_and_Apply func csRootNode.cnstrnt
	in
	cTraverseDownAll_and_Apply newFunc csRootNode
;;

(*|================================ Traverse EveryWhere ================================*)
