all: bignum bignum_tests rsa rsa_puzzle

bignum: bignum.ml
	ocamlbuild -use-ocamlfind bignum.byte

rsa: rsa.ml
	ocamlbuild -use-ocamlfind rsa.byte

rsa_puzzle: rsa_puzzle.ml
	ocamlbuild -use-ocamlfind rsa_puzzle.byte

bignum_tests: bignum_tests.ml
	ocamlbuild -use-ocamlfind bignum_tests.byte

clean:
	rm -rf _build *.byte
