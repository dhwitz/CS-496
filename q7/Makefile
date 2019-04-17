.PHONY: all clean byte test repl utop

OCB_FLAGS = -tag bin_annot -use-menhir -use-ocamlfind -pkgs oUnit
OCB = ocamlbuild $(OCB_FLAGS)

all: check interp

clean:
	$(OCB) -clean

interp:
	$(OCB) interp.byte

check:
	$(OCB) checker.byte

repl: byte
	./repl.byte

utop: byte
	utop

test:
	$(OCB) test.byte && ./test.byte
