all: ps6 draw tiles mazes puzzledesc puzzlesolve tests experiments

ps6: collections.ml
	ocamlbuild -use-ocamlfind collections.byte

draw: draw.ml
	ocamlbuild -use-ocamlfind draw.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte

tiles: tiles.ml
	ocamlbuild -use-ocamlfind tiles.byte

mazes: mazes.ml
	ocamlbuild -use-ocamlfind mazes.byte

puzzlesolve: puzzlesolve.ml
	ocamlbuild -use-ocamlfind puzzlesolve.byte

puzzledesc: puzzledescription.ml
	ocamlbuild -use-ocamlfind puzzledescription.byte

experiments: experiments.ml
	ocamlbuild -use-ocamlfind experiments.byte

clean:
	rm -rf _build *.byte
