open Graphic;;
open Action;;

open Otype;;
open Oxml;;
open Oval;;

open Core_xml;;

open Game_object;;


(** xml part *)

(** metatype *)
class xml_game_object_mt_parser=
object(self)
  inherit xml_parser

  val mutable args_parser=new xml_val_ext_list_parser "args"
  val mutable props_parser=new xml_val_ext_list_parser "properties"
  val mutable nm=""

  val mutable lua=""

  val mutable graphics_a=Hashtbl.create 2
  val mutable states_a=Hashtbl.create 2

  method get_val=(nm,args_parser#get_val,props_parser#get_val, graphics_a,states_a,lua)
  
  method parse_attr k v=
    match k with
      | "name" ->nm<-v
      | _ -> ()


  method parse_child k v=
    args_parser#parse_child k v;
    props_parser#parse_child k v;
    match k with
      | "graphics" ->
	  print_string "graphics meta";print_newline();
	  let p=new xml_graphics_mt_parser in
	    p#parse v;
	    graphics_a<-p#get_hash
      | "state_actions" ->
	  print_string "states meta";print_newline();
	  let p=new xml_states_mt_parser in
	    p#parse v;
	    states_a<-p#get_hash
      | "script" -> lua<-v#get_pcdata;

      | _ -> ()
end;;

let game_object_metatype_from_xml f=
  let obj_mt_xml=new xml_node (Xml.parse_file f) in
  let pmt=new xml_game_object_mt_parser in
    pmt#parse obj_mt_xml;
    pmt#get_val

(** object *)

class xml_game_object_parser=
object(self)
  inherit [game_object] xml_object_parser (fun()->new game_object) as super
  val mutable props_parser=new xml_val_ext_list_parser "properties"

  val mutable graphics_parser=(Global.get xml_default_graphics_parser)()
  val mutable states_parser=new xml_state_actions_parser    

  val mutable mt=("",new val_ext_handler,new val_ext_handler, Hashtbl.create 2,Hashtbl.create 2,"")
  method set_metatype m=mt<-m 

  
  method get_type=nm

 
  method parse_attr k v=
    match k with
      | "metatype"->mt<-game_object_metatype_from_xml v
      | "name"->nm<-v
      | _ -> ()
  method parse_child k v=
    let (nm,vha,vhp,grh,sth,l)=mt in
    super#parse_child k v;
    props_parser#parse_child k v;
    match k with
      | "graphics" ->
	  graphics_parser#set_metatype ("",grh,"");
	  graphics_parser#parse v;	  
      | "state_actions" ->
	  states_parser#set_metatype ("",sth,"");
	  states_parser#parse v;
      | _ -> ()

  method get_val=
    let ofun()=
      let o=
	  new game_object
      in
      let args=args_parser#get_val in
      let (gw,gh)=size_of_val (args#get_val (`String "pixel_size")) and
	  (w,h)=size_of_val (args#get_val (`String "case_size")) in
	o#set_name id;
	o#get_rect#set_size w h;
	o#get_prect#set_size gw gh;
	graphics_parser#init (o#get_graphics#add_graphic);
	states_parser#init (o#get_states#add_state);
	o#set_props props_parser#get_val;
	let (nm,vha,vhp,grh,sth,l)=mt in
	o#set_lua_script (l^lua);
	o#lua_init();
(*	self#init_object o; *)
	o	  
    in      
      (nm,ofun)

end;;

(** object types *)

class xml_game_object_types_parser=
object(self)
  inherit [(unit->game_object)] xml_stringhash_parser "game_object" (fun()->new xml_game_object_parser)
end;;


let init_game_object_types_from_xml f add_obj=
  let obj_mt_xml=new xml_node (Xml.parse_file f) in
  let pmt=new xml_game_object_types_parser in
    pmt#parse obj_mt_xml;
    let h=pmt#get_hash in
      Hashtbl.iter (
	fun k v ->
	  add_obj k v
      ) h;

