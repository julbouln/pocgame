OCAMLMAKEFILE = OCamlMakefile

INCDIRS=../poclow ../poccore ../extlib-1.3 ../xml-light2.1
LIBS=poclow poccore xml-light extLib str

# obj_type.ml -> game_object_type.ml
#   layer.ml -> game_layer.ml
#   object_layer.ml -> game_object.ml, game_object_layer.ml
#   tile_layer.ml -> game_tile_layer.ml
#   main.ml -> game_main.ml


SOURCES = locales/locale.ml locales/fr.ml game_object.ml game_layer.ml game_object_layer.ml game_visual.ml game_tile_layer.ml game_xml.ml game_decor.ml game_main.ml

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