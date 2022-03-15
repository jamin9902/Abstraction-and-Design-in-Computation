all: order orderedcoll prioqueue

order: order.ml
	ocamlbuild -use-ocamlfind order.byte

orderedcoll: orderedcoll.ml
	ocamlbuild -use-ocamlfind orderedcoll.byte

prioqueue: prioqueue.ml
	ocamlbuild -use-ocamlfind prioqueue.byte

clean:
	rm -rf _build *.byte
