OCB=ocamlbuild -use-ocamlfind -pkgs netstring,camomile,batteries

all: preprocess.native ner.native

%.native: $(wildcard *.mll *.ml)
	$(OCB) $@

%.byte: $(wildcard *.mll *.ml)
	$(OCB) $@

clean:
	$(OCB) -clean
