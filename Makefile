EXEC=serveur

CC=ocamlc
FLAGS=str.cma unix.cma threads.cma -thread -annot

SRC=protocol.ml parser.ml lexer.ml serveur_bis.ml
#SRC=protocol.ml parser.ml lexer.ml serveur.ml
OBJ=$(SRC:.ml=.cmo)

all: $(EXEC)

run: all
	./$(EXEC)

$(EXEC): $(OBJ)
	$(CC) $(FLAGS) -o $@ $^

%.cmi: %.mli
	$(CC) $(FLAGS) -c $<

%.cmo: %.ml
	$(CC) $(FLAGS) -c $<

%.ml:%.mll
	ocamllex $<

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

serveur.cmo: parser.cmi parser.cmo lexer.cmo

# depend:
# 	@echo [OCAMLDEP]
# 	ocamldep *.ml *.mli *.mly *.mll > .depend

# -include .depend

clean:
	rm -f *.cm[io] $(EXEC) *~ parser.ml parser.mli lexer.ml *.annot

.PHONY: all clean depend

lexer.cmo: parser.cmi 
parser.cmo: protocol.cmo parser.cmi
protocol.cmo: 
serveur.cmo: protocol.cmo parser.cmi lexer.cmo 
parser.cmi: protocol.cmo 
