OCAMLMAKEFILE = OCamlMakefile

INCDIRS=../poclow ../poccore ../extlib-1.3 ../xml-light2.1
LIBS=poclow poccore xml-light extLib str

SOURCES = locales/locale.ml locales/fr.ml obj_type.ml layer.ml obj_layer.ml tile_layer.ml main.ml

OCAMLOPT=ocamlopt.opt

RESULT  = pocgame

#THREADS=yes

# PREDS="str unix xml-light extLib"
 
all : ncl

#VERSION=0.12
#game_version.ml: Makefile
#	echo "let version = \""$(VERSION)"\"" > game_version.ml
#	echo "let date = \""`date`"\"" >> game_version.ml



include $(OCAMLMAKEFILE)