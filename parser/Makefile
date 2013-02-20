.PHONY: all clean lexer parser semtop

OC=ocamlbuild
OC_FLAGS=-use-ocamlfind -lib unix -use-menhir

all: lexer parser

lexer:
	@$(OC) $(OC_FLAGS) lexer.byte

parser:
	@$(OC) $(OC_FLAGS) parser.byte

semtop:
	@$(OC) $(OC_FLAGS) sem.top
	rlwrap ./sem.top -I _build

clean:
	@$(OC) -clean

