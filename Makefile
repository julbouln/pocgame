OCAMLMAKEFILE = OCamlMakefile

#INCDIRS=../oxinclude 
#LIBS=xinclude xml-light str


PACKS=pociface oxinclude

LIBINSTALL_FILES=*.cmi *.cmx *.a pocgame.cmxa
#LIB_PACK_NAME=pocgame

SOURCES = game_object.ml game_layer.ml game_object_layer.ml game_visual.ml game_tile_layer.ml game_xml.ml game_decor.ml game_loading.ml game_map.ml game_engine.ml

RESULT  = pocgame

THREADS=yes

# PREDS="str unix xml-light extLib"
 
all : ncl

#VERSION=0.12
#game_version.ml: Makefile
#	echo "let version = \""$(VERSION)"\"" > game_version.ml
#	echo "let date = \""`date`"\"" >> game_version.ml



include $(OCAMLMAKEFILE)