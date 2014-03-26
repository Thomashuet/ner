OCB=ocamlbuild -use-ocamlfind -pkgs netstring,camomile,batteries

all: preprocess.native

%.native:
	$(OCB) $@
