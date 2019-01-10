COMPILE = ocamlc
COMPILEOPT = ocamlopt
CSLDEP=ocamldep
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLDOC= ocamldoc


NORM_FILES= graphics.cma intersection.ml type.ml conversion.ml init.ml interface.ml
NORM_OBJS =  $(NORM_FILES:.ml=.cmo)
OPT_OBJS=  $(NORM_FILES:.ml=.cmx)

all : main.cma main.cmxa doc

doc:
	mkdir doc;$(CAMLDOC) -html -t "Plus court chemin" -d doc -intro Intro -hide Pervasives /mli/*.mli

main.cma: $(NORM_OBJS)
	$(COMPILE) -a -o $@ $(NORM_OBJS)

main.cmxa: $(OPT_OBJS)
	$(COMPILEOPT) -a -o $@ $(OPT_OBJS)

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.mll.mli:
	$(CAMLLEX) $<
.mll.ml:
	$(CAMLLEX) $<
.mly.mli:
	$(CAMLYACC) $<
.mly.ml:
	$(CAMLYACC) $<
.ml.cmo :
	$(COMPILE) -c $<
.mli.cmi :
	$(COMPILE) -c $<
.ml.cmx :
	$(COMPILEOPT) -c $<

cleandoc:
	\rm -Rf doc

clean:
#	\rm -f *.cmo *.cmi *.cmx *.o *.a *.cma *.cmxa .depend *~ parser.ml parser.mli lexer.ml lexer.mli
	\rm -f main.cmo main.cmi main.cmx main.o *.a *.cma *.cmxa .depend *~ parser.ml parser.mli lexer.ml lexer.mli

.depend:
	$(CSLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend
