# Makefile for yixin's autoFP Prototype Tool

# You must set the CIL environment variable for this to work. It should
# point to the directory with cil.spec in it. Mine is:
# /home/star/yixin/cil-1.3.7/cil-1.3.7 

OCAML_OPTIONS = \
  -I $(CIL)/ \
  -I $(CIL)/src \
  -I $(CIL)/src/ext \
  -I $(CIL)/src/frontc \
  -I $(CIL)/obj/x86_LINUX # NOTE: on Mac change x86_LINUX to x86_DARWIN

OCAMLC =        ocamlc                          $(OCAML_OPTIONS)
OCAMLOPT =      ocamlopt                        $(OCAML_OPTIONS)
OCAMLDEP =      ocamldep                        $(OCAML_OPTIONS)
OCAMLLEX =      ocamllex 

all: astop

%.cmo: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLC) -c -g $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

%.cmx: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLOPT) -c $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

%.cmi: %.mli
	$(OCAMLC) -c -g $*.mli

%.ml: %.mll
	$(OCAMLLEX) $*.mll





ASTOP_MODULES = \
  astop.cmo \

astop: $(ASTOP_MODULES:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ unix.cmxa str.cmxa cil.cmxa $^


# dependencies
ALL_MODULES = \
  $(MAIN_MODULES) 

-include $(ALL_MODULES:.cmo=.d)

clean:
	rm -f *.cmo *.cmi *.d *.cmx *.dx *.o astop
