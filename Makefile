OCAMLMAKEFILE = OCamlMakefile

PACKS=pociface oxinclude

LIBINSTALL_FILES=*.cmi *.cmx *.a pocgame.cmxa

SOURCES = game_object.ml game_layer.ml game_object_layer.ml game_visual.ml game_tile_layer.ml game_xml.ml game_decor.ml game_loading.ml game_map.ml game_engine.ml

RESULT  = pocgame

THREADS=yes

OCAMLDOC=ocamlfind ocamldoc -package "$(PACKS)"
DOC_FILES=$(SOURCES)

all : ncl

include $(OCAMLMAKEFILE)