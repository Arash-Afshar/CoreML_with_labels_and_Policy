
# pre req
syntax.cmo:
	ocamlc -g -c syntax.ml;

reduce.cmo:
	ocamlc -g -c reduce.ml;

unify.cmo:
	ocamlc -g -c unify.ml;

typescheme.cmo:
	ocamlc -g -c typescheme.ml;

poly.cmo: unify.cmo
	ocamlc -g -c poly.ml;

inferDefs.cmo:
	ocamlc -g -c inferDefs.ml;

aux.cmo: prettyPrinter.cmo inferDefs.cmo constraintUnify.cmo 
	ocamlc -g -c aux.ml

unificationFuncs:
	ocamlc -g -c ./unifyFunctions/delNOLs.ml;
#	ocamlc -g -c ./unifyFunctions/deleteRedundant.ml;
	ocamlc -g -c ./unifyFunctions/expressionUnification.ml;
	cp ./unifyFunctions/expressionUnification.cmi ./
	cp ./unifyFunctions/expressionUnification.cmo ./
#	cp ./unifyFunctions/deleteRedundant.cmi ./
#	cp ./unifyFunctions/deleteRedundant.cmo ./
	cp ./unifyFunctions/delNOLs.cmi ./
	cp ./unifyFunctions/delNOLs.cmo ./
	ocamlc -g -c ./unifyFunctions/typeUnification.ml;

constraintUnify.cmo: unificationFuncs
	cp ./unifyFunctions/typeUnification.cmi ./
	cp ./unifyFunctions/typeUnification.cmo ./
	ocamlc -g -c constraintUnify.ml;

constraintPolyInfer.cmo: syntax.cmo prettyPrinter.cmo inferDefs.cmo constraintUnify.cmo aux.cmo 
	ocamlc -g -c constraintPolyInfer.ml;

all.cmo:
	ocamlc -g -c all.ml;

lexer.ml:
	ocamllex lexer.mll

parser.ml:
	ocamlyacc parser.mly

parser.cmi: syntax.cmo parser.ml
	ocamlc -g -c parser.mli syntax.cmo

lexer.cmo: lexer.ml
	ocamlc -g -c lexer.ml

parser.cmo: parser.ml
	ocamlc -g -c parser.ml

prettyPrinter.cmo: syntax.cmo reduce.cmo inferDefs.cmo
	ocamlc -g -c prettyPrinter.ml

compiler.cmo: parser.cmi lexer.cmo parser.cmo syntax.cmo reduce.cmo prettyPrinter.cmo inferDefs.cmo constraintUnify.cmo constraintPolyInfer.cmo
	ocamlc -g -c compiler.ml

#main
clean:
	rm all;	rm compiler; rm *.cmo; rm *.cmi; rm parser.ml; rm parser.mli; rm lexer.ml; rm ./unifyFunctions/*.cmo; rm ./unifyFunctions/*.cmi

#body: syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prettyPrinter.cmo all.cmo
#	ocamlc -g -o all syntax.cmo reduce.cmo unify.cmo typescheme.cmo prettyPrinter.cmo all.cmo


all: compiler.cmo
	ocamlc -g -o compiler syntax.cmo inferDefs.cmo reduce.cmo prettyPrinter.cmo expressionUnification.cmo typeUnification.cmo constraintUnify.cmo aux.cmo constraintPolyInfer.cmo delNOLs.cmo lexer.cmo parser.cmo compiler.cmo 


#all: syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo
#	ocamlc -g -o all syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo

