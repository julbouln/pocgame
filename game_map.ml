
open Medias;;
open Rect;;
open File;;

open Oxml;;

open Game_xml;;
open Game_object;;
open Game_object_layer;;
open Game_tile_layer;;
open Game_decor;;

open Game_loading;;

class virtual game_object_map_actions=
object
  method virtual map_update:unit->unit

  method virtual map_add_object:string option->string->int->int->string
  method virtual map_copy_object :string option -> string -> string
  method virtual map_move_object:string->int->int->unit
  method virtual map_del_object:string->unit

  method virtual map_is_object:string->bool

  method virtual map_to_save:(string*string*string*int*int) array
  method virtual map_from_load: (string*string*string*int*int) array->unit


end;;


class ['a] game_object_map (iv:'a) wi hi max =
object(self)
  inherit ['a] game_obj_layer_hash iv wi hi max
  inherit game_object_map_actions

  method is_obj_with_type t=
    let r=ref false in
    self#foreach_object
      ( fun i o->
	  if o#get_name=t then r:=true
      );
      !r

  method get_objs_xml_string f=
    let n=xml_reduce (Xml.parse_file f) (fun n->
					     self#is_obj_with_type (Xml.attrib n "name")
					  ) in
    let s=Xml.to_string n in
      s


  method foreach_objs_xml_from_string f d=
    let decor_xml=new xml_node (Xml.parse_string f) in
    let p=new xml_decors_parser in p#parse decor_xml;
      let res=Array.of_list p#get_list in
	Array.iteri (
	  fun r v-> 
	    let nm=v.oname in
	      d nm v;
	) res;

  method foreach_objs_xml f d=
    let decor_xml=new xml_node (Xml.parse_file f) in
    let p=new xml_decors_parser in p#parse decor_xml;
      let res=Array.of_list p#get_list in
	Array.iteri (
	  fun r v-> 
	    let nm=v.oname in
	      d nm v;
	) res;
  

  val mutable obj_type=new game_obj_types iv
  method get_obj_type=obj_type
    
  method add_object_type nm (t:unit->'a)=
    obj_type#add_object_type nm t
  method get_object_from_type nm=
    obj_type#get_object_type nm

(* loading part *)
  val mutable m=Mutex.create();
  val mutable cond=Condition.create();

  val mutable load_obj_type=new game_loading_info
  method get_load_obj_type=load_obj_type



  method object_types_from_xml_string_func (n:string) (f:string) (fu:string->string->string->int->int->int->int->game_state_container->(string*(unit->'a)))=
    let obj_xml=new xml_node (Xml.parse_string f) in
    let p=new xml_game_objs_parser n iv fu in p#parse obj_xml;
	Array.iter (
	  fun v-> 
	    self#add_object_type (fst v) (snd v);
	    load_obj_type#set_data (LData (fst v));
(*
(fun nm t f w h cw ch stc->new game_decor nm w h f cw ch stc)
*)
	) p#get_objs;

	load_obj_type#set_data (LEnd);

  method object_types_from_xml_func (n:string) (f:string) (fu:string->string->string->int->int->int->int->game_state_container->(string*(unit->'a)))=
    let obj_xml=new xml_node (Xml.parse_file f) in
    let p=new xml_game_objs_parser n iv fu in p#parse obj_xml;
	Array.iter (
	  fun v-> 
	    self#add_object_type (fst v) (snd v);
	    load_obj_type#set_data (LData (fst v));
(*
(fun nm t f w h cw ch stc->new game_decor nm w h f cw ch stc)
*)
	) p#get_objs;

	load_obj_type#set_data (LEnd);

  method add_object_at (o:'a) (x:int) (y:int)=
    o#move x y;
    let n=self#add_object_with_num o in
    let id=("object"^string_of_int n) in
      o#set_id id;
      self#add_hash id n;id
	  
  method add_object_from_type (t:string) (x:int) (y:int)=
    let o=self#get_object_from_type t in
      o#set_name t;
      self#add_object_at o x y


(* map actions *)
 
  method map_update()=
    self#clean();
    self#update_obj_all();
    self#update_action(); 

  method map_add_object id t x y=
    let n=self#add_object_from_type t x y in
      print_string ("GAME_MAP: add object "^n);print_newline();
      match id with
	| Some nid ->self#map_rename_object n nid;nid
	| None -> n    

  method map_move_object id x y=
(*    print_string ("GAME_MAP: move object "^id);print_newline(); *)
    let o=self#get_hash_object id in
      o#move x y

  method map_copy_object cid id=
    let o=self#get_hash_object id in
    let no=(self#get_object_from_type o#get_name) in
    let n=self#add_object_at no o#get_rect#get_x o#get_rect#get_y in
      match cid with
	| Some nid ->self#map_rename_object n nid;nid
	| None -> n

  method map_rename_object cid id=
    print_string ("GAME_MAP: rename object "^cid^" to "^id);print_newline();
    let o=self#get_hash_object cid in
      o#set_id id;
      self#replace_hash cid id

  method map_del_object id=
    print_string ("GAME_MAP: del object "^id);print_newline();
    self#del_hash_object id

  method map_is_object id=
    self#is_hash id


(* persistence *)

 method map_to_save=
    let a=DynArray.create() in
      self#foreach_object (
	fun i o->
	  if o#get_name<>"none" then (
	    print_string ("GAME_MAP: save "^o#get_id^" of type "^o#get_name);print_newline();
	    DynArray.add a (o#get_id,o#get_name,o#get_lua,o#get_rect#get_x,o#get_rect#get_y);
	  )    
      );
      DynArray.to_array a

  method map_from_load (a:(string*string*string*int*int) array)=
    Array.iter (
      fun v->
	let (id,nm,lua,x,y)=v in
	  print_string ("GAME_MAP: load "^id^" of type "^nm);print_newline();  
	  let r=self#map_add_object (Some id) nm x y in ()
    ) a;
end;;


exception Game_map_action_not_found of string;;

class virtual ['tl] game_virtual_map w h=
object(self)
  val mutable map_actions=Hashtbl.create 2

  method add_map_action (s:string) (o:game_object_map_actions)=Hashtbl.add map_actions s o
  method get_map_action (s:string)=
    (try
       Hashtbl.find map_actions s
     with Not_found -> raise (Game_map_action_not_found s))
  method foreach_map_action f=
    Hashtbl.iter f map_actions
(*  val mutable decor_map=[new game_object_map (none_obj) w h 500] *)

  method virtual get_rect : rectangle

  method virtual get_tile_layer : 'tl

  method virtual resize : int -> int -> unit

(* on tile *)
  method foreach_tile f=
    self#get_tile_layer#foreach_map_entry f

  method tile_init t=

      for i=0 to self#get_rect#get_w-1 do
	for j=0 to self#get_rect#get_h-1 do
	  let mt=randomize 2 in

	    self#get_tile_layer#set_position i j (t+mt); 
	done;
      done;

  method position_blocking x y=
    if self#get_tile_layer#out_of_lay x y then true else false
(*
    else
      if self#get_decor_map#is_object x y then
	let o=self#get_decor_map#get_object_by_position x y in
	  o#get_blocking
      else false
*)

(* update layer *)

  method update()=
    self#get_tile_layer#update();
    self#foreach_map_action (fun i m->m#map_update());


(* persistance *)
  val mutable map_file=new file

  method objs_to_save=
    print_string ("GAME_MAP: save maps");print_newline();
    let a=Hashtbl.create 2 in
    self#foreach_map_action 
      (
	
	fun n act->
	  print_string ("GAME_MAP: save "^n^" map");print_newline();
	  Hashtbl.add a n act#map_to_save
      );
      a

  method objs_from_load a=
    Hashtbl.iter (
      fun n v->
	print_string ("GAME_MAP: load "^n^" map");print_newline();  
	let act=self#get_map_action n in
	  act#map_from_load v
    ) a;

  method private tile_to_save=
    (self#get_tile_layer#get_lay,self#get_tile_layer#get_border_layer_lay)

  method save f=
    map_file#save f (self#get_rect#get_w,self#get_rect#get_h,self#tile_to_save,self#objs_to_save)
      
  method private tile_from_load al=
    let (a,b)=al in
      self#get_tile_layer#set_lay a;
      self#get_tile_layer#set_border_layer_lay b;
 
  method load f =
    print_string ("GAME_MAP: load maps");print_newline();
    let (mw,mh,tile_ar,obj_ar)=map_file#load f in
      self#resize mw mh;
      self#tile_from_load tile_ar;
      self#objs_from_load obj_ar;
      ()


end;;


class game_generic_map w h=
object(self)

  val mutable rect=new rectangle 0 0 w h
  method get_rect=rect

  val mutable tile_layer=new game_generic_tile_layer w h 32 32
  method get_tile_layer=tile_layer


  method resize nw nh=
    rect<-new rectangle 0 0 nw nh;
    tile_layer<-new game_generic_tile_layer nw nh 32 32;

end;;


class game_map w h=
object(self)

  val mutable rect=new rectangle 0 0 w h
  method get_rect=rect

  val mutable tile_layer=new game_tile_layer w h 32 32 "medias/tiles/terrains.png"
  method get_tile_layer=tile_layer


(*
  val mutable decor_layer=new game_object_layer_hash w h 500
  method get_decor_layer=decor_layer
*)

  val mutable grille=new graphic_simple_object "medias/misc/grille.png"

  method put_grille vx vy x y=
    grille#move ((x*32)-vx) ((y*32)-vy);
    grille#put();
(*
  method put_decor_grille (vx:int) (vy:int)=
    decor_map#foreach_object (
      fun k obj->
	obj#around_object decor_layer#out_of_lay (
	  fun x y->
	    grille#move (x*32-vx) (y*32-vy);
	    grille#put();
	)
    )
*)

  method resize nw nh=
    rect<-new rectangle 0 0 nw nh;
    tile_layer<-new game_tile_layer nw nh 32 32 "medias/tiles/terrains.png";

end;;
