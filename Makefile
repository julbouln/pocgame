OCAMLMAKEFILE = OCamlMakefile

PACKS=poccore

LIBINSTALL_FILES=*.cmi *.cmo *.cmx *.a pocgame.cma pocgame.cmxa

SOURCES = game_object.ml game_layer.ml game_object_layer.ml game_visual.ml game_tile_layer.ml game_loading.ml game_dijkstra.ml game_pathfinding.ml game_map.ml game_engine.ml game_xml.ml

RESULT  = pocgame

THREADS=yes

OCAMLDOC=ocamlfind ocamldoc -package "$(PACKS)"
DOC_FILES=$(SOURCES)

all : ncl bcl

include $(OCAMLMAKEFILE)