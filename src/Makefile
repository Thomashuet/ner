OCB=ocamlbuild -use-ocamlfind -pkgs netstring,camomile,batteries

all: preprocess.native ner.native

%.native: $(wildcard *.mll *.ml)
	$(OCB) $@
