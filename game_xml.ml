open Value_xml;;
open Value_val;;
open Value_xinclude;;

open Core_graphic;;
open Core_action;;
open Core_type;;
open Core_xml;;
open Core_val;;

open Game_object;;


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
      
      o#set_name id; 
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


(* DEPRECATED *)
let init_game_object_types_from_xml f add_obj=
(*  let xinc=xinclude_process_file f in
  let obj_xml=new xml_node (Xml.parse_string xinc) in *)
  let obj_xml=xml_node_from_file f in
  let pmt=new xml_game_object_types_parser in
    pmt#parse obj_xml;
    let h=pmt#get_hash in
      Hashtbl.iter (
	fun k v ->
	  add_obj k v
      ) h;


