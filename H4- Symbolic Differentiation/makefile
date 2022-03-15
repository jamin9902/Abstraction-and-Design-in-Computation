all: expression expression_tests expressionLibrary

expression: expression.ml
	ocamlbuild -use-ocamlfind expression.byte

expression_tests: expression_tests.ml
	ocamlbuild -use-ocamlfind expression_tests.byte

expressionLibrary: expressionLibrary.ml
	ocamlbuild -use-ocamlfind expressionLibrary.byte

clean:
	rm -rf _build *.byte
