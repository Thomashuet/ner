OCB=ocamlbuild -use-ocamlfind

all: preprocess.native relatedness.native ner.native

%.native: $(wildcard *.mll *.ml)
	$(OCB) $@

clean:
	$(OCB) -clean
