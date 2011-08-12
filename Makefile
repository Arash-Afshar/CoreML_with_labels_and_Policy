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

constUnify.cmo:
	ocamlc -g -c constraintUnify.ml;

constPoly.cmo: constUnify.cmo
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

compiler.cmo:
	ocamlc -g -c compiler.ml

prettyPrinter.cmo:
	ocamlc -g -c prettyPrinter.ml

#main
clean: 
	rm all;	rm compiler; rm *.cmo; rm *.cmi; rm parser.ml; rm parser.mli; rm lexer.ml;

#body: syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prettyPrinter.cmo all.cmo
#	ocamlc -g -o all syntax.cmo reduce.cmo unify.cmo typescheme.cmo prettyPrinter.cmo all.cmo


compiler: syntax.cmo inferDefs.cmo reduce.cmo prettyPrinter.cmo constPoly.cmo constUnify.cmo parser.cmi  lexer.cmo parser.cmo compiler.cmo
	ocamlc -g -o compiler syntax.cmo inferDefs.cmo reduce.cmo prettyPrinter.cmo constraintPolyInfer.cmo constraintUnify.cmo lexer.cmo parser.cmo compiler.cmo 

all: body compiler
	

#all: syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo
#	ocamlc -g -o all syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo

