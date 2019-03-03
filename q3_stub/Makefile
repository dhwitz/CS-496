.PHONY: all clean byte  utop

OCB_FLAGS = -tag bin_annot -use-menhir -use-ocamlfind -pkgs oUnit
OCB = ocamlbuild $(OCB_FLAGS)

all: byte

clean:
	$(OCB) -clean

byte:
	$(OCB) interp.byte

utop: byte
	utop

