open Value_xml;;
open Value_val;;
open Value_xmlparser;;

open Core_cursor;;
open Core_stage;;
open Core_graphic;;
open Core_action;;
open Core_type;;
open Core_xml;;
open Core_val;;

open Interface;;
open Iface_object;;

open Game_object;;
open Game_tile_layer;;
open Game_map;;
open Game_engine;;
(** xml part *)


class xml_game_object_type_parser=
object(self)
  inherit [game_object] xml_object_parser (fun()->new game_object) as super
  val mutable props_parser=new xml_val_ext_list_parser "properties"

  val mutable graphics_parser=(Global.get xml_default_graphics_parser)()
  val mutable states_parser=new xml_state_actions_parser    
  
  method get_type=nm

  method init_object o=
    super#init_object o;
    let args=args_parser#get_val in
    let (gw,gh)=size_of_val (args#get_val (`String "pixel_size")) and
	(w,h)=size_of_val (args#get_val (`String "case_size")) and
	block=if args#is_val(`String "blocking") then (bool_of_val(args#get_val(`String "blocking"))) else false in
      
      o#set_name nm; 
      o#get_rect#set_size w h;
      o#get_prect#set_size gw gh;
      o#set_blocking block;
      graphics_parser#init_simple (o#get_graphics#add_graphic);
      states_parser#init_simple (o#get_states#add_state);
      o#set_props props_parser#get_val;
      o#set_lua_script (lua);
      ignore(o#lua_init());
 
  method parse_attr k v=
    match k with
      | "name"->nm<-v
      | _ -> ()

  method parse_child k v=
    super#parse_child k v;
    props_parser#parse_child k v;
    match k with
      | "graphics" ->
	  graphics_parser#parse v;	  
      | "state_actions" ->
	  states_parser#parse v;
      | _ -> ()

  method get_val=
    let ofun()=
      let o=
	  new game_object
      in
	self#init_object o;
	o	  
    in      
      (nm,ofun)

end;;

(** object types *)

class xml_game_object_types_parser=
object(self)
  inherit [(unit->game_object)] xml_stringhash_parser "game_object_type" (fun()->new xml_game_object_type_parser)
end;;



(* xml type parser *)

(* game_object_map *)

class xml_game_object_map_type_parser=
object(self)
  inherit [game_object_map] xml_object_parser (fun()->new game_object_map 0 0) as super
  val mutable obj_types_parser=new xml_game_object_types_parser

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "game_object_types" -> obj_types_parser#parse v
      | _ ->()

  method init_object o=
    o#set_lua_script lua;

    let h=obj_types_parser#get_hash in
      Hashtbl.iter (
	fun k v ->
	  o#get_obj_type#add_object_type k v;
      ) h;
(*
    o#init_object_types_from_xml (string_of_val (args_parser#get_val#get_val (`String "types")))
*)

end;;



class xml_game_object_maps_type_parser=
object(self)
  inherit [xml_game_object_map_type_parser,game_object_map] xml_container_parser "game_object_map_type" (fun()->new xml_game_object_map_type_parser) as super

  initializer
    self#parser_add "with_loading" (fun()->new xml_game_object_map_type_parser);
    self#parser_add "normal" (fun()->new xml_game_object_map_type_parser);
    



end;;

(* game_tile_layer *)

class xml_game_tile_layer_type_parser=
object(self)
  inherit [game_generic_tile_layer] xml_object_parser (fun()->new game_generic_tile_layer 0 0 32 32) as super

  method get_type="unique"


  method init_object o=
    o#set_lua_script lua;


  method get_val=
    let ofun()=
      let o=
	new game_tile_layer 0 0 32 32 (string_of_val (args_parser#get_val#get_val (`String "file")))
      in
	self#init_object (o:>game_generic_tile_layer);
	(o:>game_generic_tile_layer)	  
    in      
      (id,ofun)


end;;

class xml_game_tile_layers_type_parser=
object(self)
  inherit [xml_game_tile_layer_type_parser,game_generic_tile_layer] xml_container_parser "game_tile_layer_type" (fun()->new xml_game_tile_layer_type_parser)

  initializer
    self#parser_add "unique" (fun()->new xml_game_tile_layer_type_parser)
end;;


class xml_game_map_type_parser=
object(self)
  inherit xml_parser

  val mutable maps_type_parser=new xml_game_object_maps_type_parser
  val mutable layers_type_parser=new xml_game_tile_layers_type_parser

 
  method parse_attr k v=()

  method parse_child k v=
    match k with
      | "game_object_maps_type" ->
	  maps_type_parser#parse v;	  
	  
      | "game_tile_layers_type" ->
	  layers_type_parser#parse v;
      | _ -> ()

  method init add_map add_lay=
    maps_type_parser#init_simple (add_map);
    layers_type_parser#init_simple (add_lay);

end;;



class xml_game_engine_stage_parser=
object (self)
  inherit xml_stage_parser as super

  val mutable map_type_parser=new xml_game_map_type_parser
  val mutable interaction_parser=new xml_interaction_object_parser

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "game_map_type" -> map_type_parser#parse v 
      | "interaction"->	  interaction_parser#parse v
      | _ -> ()
(** object initial init *)
  method init_object o=
    o#set_lua_script (lua);
    

  method get_val=
    let ofun()=
      let o=
	new game_engine generic_cursor 
      in
	map_type_parser#init o#get_map#add_object_map o#get_map#add_tile_layer;
	let inter=(snd interaction_parser#get_val)() in
	  o#set_interaction inter;
	self#init_object (o:>stage); 
	(o:>stage)	  
    in      
      (id,ofun)

end;;

let xml_engine_stages_parser()=
  let p=xml_iface_stages_parser() in
    p#parser_add "game_engine" (fun()->new xml_game_engine_stage_parser);
    p;;

(*
class xml_game_engine_with_iface_stage_parser=
object (self)
  inherit xml_stage_parser as super

  val mutable map_type_parser=new xml_game_map_type_parser

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "game_map_type" -> map_type_parser#parse v 
      | _ -> ()
(** object initial init *)
  method init_object o=
    o#set_lua_script (lua);
    

  method get_val=
    let ofun()=
      let o=
	new game_engine_with_iface generic_cursor 
(*	  (string_of_val (args_parser#get_val#get_val (`String "map_type_file")))*)
	  (string_of_val (args_parser#get_val#get_val (`String "iface_file")))
(*	  (text_of_val (args_parser#get_val#get_val (`String "script"))) *)
      in
	map_type_parser#init o#get_map#add_object_map o#get_map#add_tile_layer;
	o#get_iobj#set_lua_script lua;
(*	self#init_object (o:>stage); *)
	(o:>stage)	  
    in      
      (id,ofun)

end;;

let xml_engine_stages_parser()=
  let p=xml_iface_stages_parser() in
    p#parser_add "engine_stage_with_iface" (fun()->new xml_game_engine_with_iface_stage_parser);
    p;;

*)
