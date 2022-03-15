all: music native refs_test refs sample stream

music: music.ml
	ocamlbuild -use-ocamlfind music.byte

native: nativeLazyStreams.ml
	ocamlbuild -use-ocamlfind nativeLazyStreams.byte

refs_test: refs_test.ml
	ocamlbuild -use-ocamlfind refs_test.byte

refs: refs.ml
	ocamlbuild -use-ocamlfind refs.byte

sample: sampleStreams.ml
	ocamlbuild -use-ocamlfind sampleStreams.byte

stream: streamstrees.ml	
	ocamlbuild -use-ocamlfind streamstrees.byte

clean:
	rm -rf _build *.byte
