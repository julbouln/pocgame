(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Value_xml;;
open Value_val;;
open Value_lua;;
open Value_xmlparser;;

open Core_cursor;;
open Core_stage;;
open Core_graphic;;
open Core_action;;
open Core_type;;
open Core_xml;;
open Core_val;;

open Game_object;;
open Game_tile_layer;;
open Game_map;;
open Game_interaction;;

open Game_engine;;



open Game_net;;

open Game_action;;

(** Game xml interface *)


(** action pathfinding parser *)
class xml_action_pathfinding_parser=
object(self)
  inherit [action_lua] xml_object_parser (fun()->new action_pathfinding)
end;;

let xml_game_actions_parser()=
  let p=xml_factory_actions_parser() in
    p#parser_add "action_pathfinding" (fun()->new xml_action_pathfinding_parser);
    p;;

Global.set xml_default_actions_parser xml_game_actions_parser;;

class xml_game_object_type_parser drawing_vault=
object(self)
  inherit [game_object] xml_object_parser (fun()->new game_object) as super
  val mutable props_parser=new xml_val_ext_list_parser "variables"

  val mutable graphics_parser=(Global.get xml_default_graphics_parser) drawing_vault
  val mutable states_parser=new xml_state_actions_parser    
  
  method get_type=nm

  method init_object o=
(*    super#init_object o; *)
    let args=args_parser#get_val in
    let (gw,gh)=size_of_val (args#get_val (`String "pixel_size")) and
	(w,h)=size_of_val (args#get_val (`String "case_size")) and
	block=if args#is_val(`String "blocking") then (bool_of_val(args#get_val(`String "blocking"))) else false in

    let props=props_parser#get_val in
(*      if props#is_val (`String "nom") then(
	print_string ("found prop!");print_newline());

      if args#is_val (`String "pixel_size") then(
	print_string ("found arg!");print_newline());
*)    
      o#set_name nm; 
      o#get_rect#set_size w h;
      o#get_prect#set_size gw gh;
      o#set_blocking block;
      graphics_parser#init_simple (o#get_graphics#add_graphic);
      states_parser#init_simple (o#get_states#add_state);
      o#set_props props;

(*      o#get_lua#set_val (Value_lua.OLuaVal.String "properties") (Value_lua.OLuaVal.Table props_parser#get_val#to_lua#to_table); *)
      o#set_lua_script (lua);
(*      ignore(o#lua_init()); *)
 
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
      | "states" ->
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

class xml_game_object_types_parser drawing_vault=
object(self)
  inherit [(unit->game_object)] xml_stringhash_parser "game_object_type" (fun()->new xml_game_object_type_parser drawing_vault) as super

  method init (add_obj:string->(unit->game_object)->unit)=
    Hashtbl.iter (
      fun k v->
	add_obj k v
    ) super#get_hash;


end;;




(* xml type parser *)

(* game_object_map *)

class xml_game_object_map_type_parser drawing_vault=
object(self)
  inherit [game_object_map] xml_object_parser (fun()->new game_object_map 0 0) as super
  val mutable obj_types_parser=new xml_game_object_types_parser drawing_vault

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "game_object_types" -> obj_types_parser#parse v
      | _ ->()

  method init_object o=
    o#set_lua_script lua;
    obj_types_parser#init o#get_obj_type#add_object_type;

end;;



class xml_game_object_maps_type_parser drawing_vault=
object(self)
  inherit [xml_game_object_map_type_parser,game_object_map] xml_container_parser "game_object_map_type" (fun()->new xml_game_object_map_type_parser drawing_vault) as super

  initializer
    self#parser_add "with_loading" (fun()->new xml_game_object_map_type_parser drawing_vault);
    self#parser_add "normal" (fun()->new xml_game_object_map_type_parser drawing_vault);
    
end;;

(* game_tile_layer *)

class xml_game_tile_layer_type_parser drawing_vault=
object(self)
  inherit [game_generic_tile_layer] xml_object_parser (fun()->new game_generic_tile_layer 0 0 32 32) as super

  method get_type="unique"

  method init_object o=
    o#set_lua_script lua;

  method get_val=
    let ofun()=
      let o=
	new game_tile_layer drawing_vault 0 0 32 32 (string_of_val (args_parser#get_val#get_val (`String "file")))
      in
	self#init_object (o:>game_generic_tile_layer);
	(o:>game_generic_tile_layer)	  
    in      
      (id,ofun)


end;;

class xml_game_tile_layers_type_parser drawing_vault=
object(self)
  inherit [xml_game_tile_layer_type_parser,game_generic_tile_layer] xml_container_parser "game_tile_layer_type" (fun()->new xml_game_tile_layer_type_parser drawing_vault)

  initializer
    self#parser_add "unique" (fun()->new xml_game_tile_layer_type_parser drawing_vault)
end;;


class xml_game_map_type_parser drawing_vault=
object(self)
  inherit xml_parser

  val mutable maps_type_parser=new xml_game_object_maps_type_parser drawing_vault
  val mutable layers_type_parser=new xml_game_tile_layers_type_parser drawing_vault

 
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



class xml_interaction_mouse_scroll_parser=
object(self)
  inherit xml_interaction_object_parser as super
  method get_type=nm

  method get_val=
    let ofun()=
      let args=args_parser#get_val in
      let s=(int_of_val(args#get_val (`String "scroll"))) and
	b=(int_of_val(args#get_val (`String "border"))) in	
      let o=
(* FIXME : must be scr_w scr_h *)
	new interaction_mouse_scroll s b 800 600
      in
	self#init_object o;
	o	  
    in      
      (id,ofun)

end;;


class xml_game_engine_stage_parser drawing_vault=
object (self)
  inherit xml_stage_parser drawing_vault as super

  val mutable map_type_parser=new xml_game_map_type_parser drawing_vault
  val mutable interaction_parser=(Global.get xml_default_interactions_parser)()

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "game_map_type" -> map_type_parser#parse v 
      | "interactions"->	  interaction_parser#parse v
      | _ -> ()


  method init_object o=
    super#init_object o;
    o#fun_init();

  method get_val=
    let ofun()=
      let o=
	new game_engine drawing_vault (generic_cursor drawing_vault)
      in
	map_type_parser#init o#get_map#add_object_map o#get_map#add_tile_layer;
(*	let inter=(snd interaction_parser#get_val)() in
	  o#set_interaction inter;
*)
	interaction_parser#init o#get_interaction#add_interaction;
	self#init_object (o:>stage); 
	(o:>stage)	  
    in      
      (id,ofun)

end;;

(* game world *)
class xml_game_world_engine_stage_parser drawing_vault=
object (self)
  inherit xml_stage_parser drawing_vault as super

  val mutable map_type_parser=new xml_game_map_type_parser drawing_vault
  val mutable interaction_parser=(Global.get xml_default_interactions_parser)()

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "game_map_type" -> map_type_parser#parse v 
      | "interactions"->	  interaction_parser#parse v
      | _ -> ()


  method init_object o=
    super#init_object o;
    o#fun_init();

  method get_val=
    let ofun()=
      let o=
	new game_world_engine drawing_vault (generic_cursor drawing_vault)
      in
	o#get_world#set_init_map (fun m->map_type_parser#init m#add_object_map m#add_tile_layer);
	interaction_parser#init o#get_interaction#add_interaction;
	self#init_object (o:>stage); 
	(o:>stage)	  
    in      
      (id,ofun)

end;;

open Net_xml;;

class xml_net_client_game_engine_stage_parser drawing_vault=
object (self)
  inherit xml_game_engine_stage_parser drawing_vault as super

  val mutable msg_handler_parser=new xml_net_msg_handlers_parser

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "net_message_handlers" -> msg_handler_parser#parse v 
      | _ -> ()


  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
	let saddr=(string_of_val(args#get_val (`String "server_address"))) and
	    sport=(int_of_val(args#get_val (`String "server_port"))) and
	    cport=(int_of_val(args#get_val (`String "client_port"))) in
	new net_client_game_engine drawing_vault curs saddr sport cport
      in
	map_type_parser#init o#get_map#add_object_map o#get_map#add_tile_layer;
(*	let inter=(snd interaction_parser#get_val)() in
	  o#set_interaction inter;*)
	interaction_parser#init o#get_interaction#add_interaction;
	  msg_handler_parser#init o#add_message_handler (o:>lua_object);
	self#init_object (o:>stage);
	(o:>stage)	  
    in      
      (id,ofun)

end;;


class xml_net_server_game_engine_stage_parser drawing_vault=
object (self)
  inherit xml_game_engine_stage_parser drawing_vault as super

  val mutable msg_handler_parser=new xml_net_msg_handlers_parser

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "net_message_handlers" -> msg_handler_parser#parse v 
      | _ -> ()

  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
	let sport=(int_of_val(args#get_val (`String "server_port"))) in
	  new net_server_game_engine drawing_vault sport
      in
	map_type_parser#init o#get_map#add_object_map o#get_map#add_tile_layer;
(*	let inter=(snd interaction_parser#get_val)() in
	  o#set_interaction inter;*)
	interaction_parser#init o#get_interaction#add_interaction;
	  msg_handler_parser#init o#add_message_handler (o:>lua_object);
	self#init_object (o:>stage);
	(o:>stage)	  
    in      
      (id,ofun)

end;;


class xml_net_server_game_world_engine_stage_parser drawing_vault=
object (self)
  inherit xml_game_world_engine_stage_parser drawing_vault as super

  val mutable msg_handler_parser=new xml_net_msg_handlers_parser

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "net_message_handlers" -> msg_handler_parser#parse v 
      | _ -> ()

  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
	let sport=(int_of_val(args#get_val (`String "server_port"))) in
	  new net_server_game_world_engine drawing_vault sport
      in
	o#get_world#set_init_map (fun m->map_type_parser#init m#add_object_map m#add_tile_layer);
	interaction_parser#init o#get_interaction#add_interaction;
	  msg_handler_parser#init o#add_message_handler (o:>lua_object);
	self#init_object (o:>stage);
	(o:>stage)	  
    in      
      (id,ofun)

end;;


let xml_game_interactions_parser()=
  let p=xml_generic_interactions_parser() in
    p#parser_add "interaction_mouse_scroll" (fun()->new xml_interaction_mouse_scroll_parser);
    p;;

Global.set xml_default_interactions_parser  xml_game_interactions_parser;;


let xml_engine_stages_parser drawing_vault=
  let p=xml_factory_stages_parser drawing_vault in
    p#parser_add "game_engine" (fun()->new xml_game_engine_stage_parser drawing_vault);
    p#parser_add "game_world" (fun()->new xml_game_world_engine_stage_parser drawing_vault);
    p#parser_add "net_client_game_engine" (fun()->new xml_net_client_game_engine_stage_parser drawing_vault);
    p#parser_add "net_server_game_engine" (fun()->new xml_net_server_game_engine_stage_parser drawing_vault);
    p#parser_add "net_server_game_world" (fun()->new xml_net_server_game_world_engine_stage_parser drawing_vault);
    p;;

