open Syntax;;
open Reduce;;
open Unify;;
open Typescheme;;
open Poly;;
open PrettyPrinter;;


let e =  
LetP ( ["addLabel"; "lab"; "exp"] , App( App( Const { name = Name "addLab"; constr = false; arity = 2} , Var "lab") , Var "exp") , LetP ( ["lift"; "a"] , App( App( App( Const { name = Name "branch"; constr = false; arity = 3} , App( App( Const { name = Name "=="; constr = false; arity = 2} , App( Const { name = Name "getLab"; constr = false; arity = 1} , Var "a")) , Const { name = Lab "low"; constr = false; arity = 0})) , App( App( Const { name = Name "addLab"; constr = false; arity = 2} , Const { name = Lab "high"; constr = false; arity = 0}) , App( Const { name = Name "remLab"; constr = false; arity = 1} , Var "a"))) , Var "a") , App( Var "lift" , App( App( Fun ("x" , Var "x") , App( Var "addLabel" , Const { name = Lab "low"; constr = false; arity = 0})) , Const { name = Int 1; constr = true; arity = 0}))));;

(*
type_of e;;
eval [] "code" e;;
*)


print_answer ( eval [] "code" e ); print_newline();;






