open InferDefs;;
open PrettyPrinter;;
open Syntax;;

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++ Delete NOLs ++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

let rec addBatchLabels labelList labelAcceptingPart =
	let length = (List.length labelList) in
	if (length != 0) then
		(tlabeled labelAcceptingPart (List.hd labelList))
	else
		labelAcceptingPart
;;

let rec addLabelsToType labelList unlabeledPart parent =
parent.texp_node <- (addBatchLabels labelList unlabeledPart).texp_node
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

(**
	To solve the problem of label-free variables (rule T-UNLAB), if a label-free variable is found, a NOL will be added to it.
*)
(*
let del_notAll_NOLs labelList resultList =
	let res = delNOLs labelList resultList in
	if (List.length res) == 0 then
		[nol]
	else
		res
;;*)

let del_notAll_NOLs labelList resultList =
	if (List.length labelList) == 0 then
		[nol]
	else
		labelList
;;

let rec delNOLsTE (Tnode te) =
	match te.texp_node with
	  Tlabeled (t, e) ->
		let labelList = getAllLabels te in
		let newLabelList = del_notAll_NOLs labelList [] in
		let unlabeledPart = texp (getUnlabeledPart te) in
		addLabelsToType newLabelList unlabeledPart te;
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
(* ==================================================== Delete NOLs ================================================== *)
