all: mapfold mapfold_tests

mapfold: mapfold.ml
	ocamlbuild -use-ocamlfind mapfold.byte

mapfold_tests: mapfold_tests.ml
	ocamlbuild -use-ocamlfind mapfold_tests.byte

clean:
	rm -rf _build *.byte
