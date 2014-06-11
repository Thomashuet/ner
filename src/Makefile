OCB=ocamlbuild -use-ocamlfind

all: preprocess.native similarity.native ner.native

%.native: $(wildcard *.mll *.ml)
	$(OCB) $@

clean:
	$(OCB) -clean
