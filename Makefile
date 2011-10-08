
# pre req
syntax.cmo:
	ocamlc.opt -g -c syntax.ml;

reduce.cmo:
	ocamlc.opt -g -c reduce.ml;

unify.cmo:
	ocamlc.opt -g -c unify.ml;

typescheme.cmo:
	ocamlc.opt -g -c typescheme.ml;

poly.cmo: unify.cmo
	ocamlc.opt -g -c poly.ml;

inferDefs.cmo:
	ocamlc.opt -g -c inferDefs.ml;

aux.cmo: prettyPrinter.cmo inferDefs.cmo constraintUnify.cmo 
	ocamlc.opt -g -c aux.ml

unificationFuncs:
	ocamlc.opt -g -c ./unifyFunctions/delNOLs.ml;
#	ocamlc.opt -g -c ./unifyFunctions/deleteRedundant.ml;
	ocamlc.opt -g -c ./unifyFunctions/expressionUnification.ml;
	cp ./unifyFunctions/expressionUnification.cmi ./
	cp ./unifyFunctions/expressionUnification.cmo ./
#	cp ./unifyFunctions/deleteRedundant.cmi ./
#	cp ./unifyFunctions/deleteRedundant.cmo ./
	cp ./unifyFunctions/delNOLs.cmi ./
	cp ./unifyFunctions/delNOLs.cmo ./
	ocamlc.opt -g -c ./unifyFunctions/typeUnification.ml;

constraintUnify.cmo: unificationFuncs
	cp ./unifyFunctions/typeUnification.cmi ./
	cp ./unifyFunctions/typeUnification.cmo ./
	ocamlc.opt -g -c constraintUnify.ml;

constraintPolyInfer.cmo: syntax.cmo prettyPrinter.cmo inferDefs.cmo constraintUnify.cmo aux.cmo 
	ocamlc.opt -g -c constraintPolyInfer.ml;

all.cmo:
	ocamlc.opt -g -c all.ml;

lexer.ml:
	ocamllex lexer.mll

parser.ml:
	ocamlyacc parser.mly

parser.cmi: syntax.cmo parser.ml
	ocamlc.opt -g -c parser.mli syntax.cmo

lexer.cmo: lexer.ml
	ocamlc.opt -g -c lexer.ml

parser.cmo: parser.ml
	ocamlc.opt -g -c parser.ml

prettyPrinter.cmo: syntax.cmo reduce.cmo inferDefs.cmo
	ocamlc.opt -g -c prettyPrinter.ml

compiler.cmo: parser.cmi lexer.cmo parser.cmo syntax.cmo reduce.cmo prettyPrinter.cmo inferDefs.cmo constraintUnify.cmo constraintPolyInfer.cmo
	ocamlc.opt -g -c compiler.ml

#main
clean:
	rm all;	rm compiler; rm *.cmo; rm *.cmi; rm parser.ml; rm parser.mli; rm lexer.ml; rm ./unifyFunctions/*.cmo; rm ./unifyFunctions/*.cmi

#body: syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prettyPrinter.cmo all.cmo
#	ocamlc.opt -g -o all syntax.cmo reduce.cmo unify.cmo typescheme.cmo prettyPrinter.cmo all.cmo


all: compiler.cmo
	ocamlc.opt -g -o compiler syntax.cmo inferDefs.cmo reduce.cmo prettyPrinter.cmo expressionUnification.cmo typeUnification.cmo constraintUnify.cmo aux.cmo constraintPolyInfer.cmo delNOLs.cmo lexer.cmo parser.cmo compiler.cmo 


#all: syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo
#	ocamlc.opt -g -o all syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo

