all: statistics utilities visualization run registry counter people config

statistics: statistics.ml
	ocamlbuild -use-ocamlfind statistics.byte

utilities: utilities.ml
	ocamlbuild -use-ocamlfind utilities.byte

visualization: visualization.ml
	ocamlbuild -use-ocamlfind visualization.byte

run: run.ml
	ocamlbuild -use-ocamlfind run.byte

registry: registry.ml
	ocamlbuild -use-ocamlfind registry.byte

counter: counter.ml
	ocamlbuild -use-ocamlfind counter.byte

people: people.ml
	ocamlbuild -use-ocamlfind people.byte

config: config.ml
	ocamlbuild -use-ocamlfind config.byte

clean:
	rm -rf _build *.byte
