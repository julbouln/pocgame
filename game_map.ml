
open Medias;;
open Graphic;;
open Rect;;
open File;;

open Olua;;

open Oxml;;

open Game_xml;;
open Game_object;;
open Game_object_layer;;
open Game_tile_layer;;
open Game_decor;;

open Game_loading;;

class game_object_map wi hi=
object(self)
  inherit lua_object as lo
  inherit [game_object] game_obj_layer wi hi


  method is_obj_with_type t=
    let r=ref false in
      self#foreach_object
	( fun i o->
	    if o#get_name=t then r:=true
	);
      !r
	
  val mutable obj_type=new game_object_types
  method get_obj_type=obj_type
    
  method add_object_type nm (t:unit->'a)=
    obj_type#add_object_type nm t
  method get_object_from_type nm=
    obj_type#get_object_type nm

  (** xml *)
(*
  method get_objs_xml_string f=
    let n=xml_reduce (Xml.parse_file f) (fun n->
					   self#is_obj_with_type (Xml.attrib n "name")
					) in
    let s=Xml.to_string n in
      s


  method foreach_objs_xml_from_string f d=
    let decor_xml=new xml_node (Xml.parse_string f) in
    let p=new xml_gm_objects_parser "game_object" in p#parse decor_xml;
      let res=Array.of_list p#get_list in
	Array.iteri (
	  fun r v-> 
	    let nm=v.oname in
	      d nm v;
	) res;

  method foreach_objs_xml f d=
    let decor_xml=new xml_node (Xml.parse_file f) in
    let p=new xml_gm_objects_parser "game_object" in p#parse decor_xml;
      let res=Array.of_list p#get_list in
	Array.iteri (
	  fun r v-> 
	    let nm=v.oname in
	      d nm v;
	) res;
  

  method object_types_from_xml_string_func (n:string) (f:string) (fu:string->string->string->int->int->int->int->game_state_container->(string*(unit->'a)))=
    let obj_xml=new xml_node (Xml.parse_string f) in
    let p=new xml_game_objs_parser n fu in p#parse obj_xml;
      Array.iter (
	fun v-> 
	  self#add_object_type (fst v) (snd v);
	  (*
	    (fun nm t f w h cw ch stc->new game_decor nm w h f cw ch stc)
	  *)
      ) p#get_objs;


  method object_types_from_xml_func (n:string) (f:string) (fu:string->string->string->int->int->int->int->game_state_container->(string*(unit->'a)))=
    let obj_xml=new xml_node (Xml.parse_file f) in
    let p=new xml_game_objs_parser n fu in p#parse obj_xml;
      Array.iter (
	fun v-> 
	    self#add_object_type (fst v) (snd v);
	  (*
	    (fun nm t f w h cw ch stc->new game_decor nm w h f cw ch stc)
	  *)
      ) p#get_objs;

*)
  method init_object_types_from_xml f=
    obj_type#init_from_xml f

(** general *)

  method add_object_at (id:string option) (o:'a) (x:int) (y:int)=    

    o#move x y;
    let n=self#add_object id o in
      print_string ("GAME_OBJECT_MAP: add object "^n);print_newline();
      o#lua_init();
      self#lua_parent_of n (o:>lua_object);
      n
	  
  method update()=
    self#clear();
    self#update_obj_all();
    self#update_action(); 


  method add_object_from_type id t x y=
    let o=self#get_object_from_type t in
      o#set_name t;
      self#add_object_at id o x y 

  method copy_object cid id=
    let o=self#get_object id in
    let no=(self#get_object_from_type o#get_name) in
    self#add_object_at cid no o#get_rect#get_x o#get_rect#get_y

(* persistence *)

  method map_to_save=
    let a=DynArray.create() in
      self#foreach_object (
	fun i o->
	  if o#get_name<>"none" then (
	    print_string ("GAME_OBJECT_MAP: save "^o#get_id^" of type "^o#get_name);print_newline();
	    DynArray.add a (o#get_id,o#get_name,o#get_rect#get_x,o#get_rect#get_y);
	  )    
      );
      DynArray.to_array a

 method map_from_load (a:(string*string*int*int) array)=
   Array.iter (
     fun v->
       let (id,nm,x,y)=v in
	 print_string ("GAME_OBJECT_MAP: load "^id^" of type "^nm);print_newline();  
	 let r=self#add_object_from_type (Some id) nm x y in ()
   ) a;


 method lua_init()=
   lo#lua_init();

end;;


exception Game_object_map_not_found of string;;
exception Game_tile_layer_not_found of string;;

class game_map w h=
object(self)
  inherit lua_object as lo

  val mutable rect=new rectangle 0 0 w h
  method get_rect=rect

  method resize nw nh=
    self#foreach_tile_layer 
      ( 
	fun k t->
	  t#resize nw nh
      );
    self#foreach_object_map 
      ( 
	fun k om->
	  om#resize nw nh
      );
    rect#set_size nw nh;

  val mutable tile_layers=Hashtbl.create 2

  method add_tile_layer (s:string) (o:game_generic_tile_layer)=
    Hashtbl.add tile_layers s o

  method get_tile_layer (s:string)=
    (try
       Hashtbl.find tile_layers s
     with Not_found -> raise (Game_tile_layer_not_found s))

  method foreach_tile_layer f=
    Hashtbl.iter f tile_layers

  (* map actions *)
  val mutable object_maps=Hashtbl.create 2
    
  method add_object_map (s:string) (o:game_object_map)=
    self#lua_parent_of s (o:>lua_object);
    Hashtbl.add object_maps s o

  method get_object_map (s:string)=
    (try
       Hashtbl.find object_maps s
     with Not_found -> raise (Game_object_map_not_found s))

  method foreach_object_map f=
    Hashtbl.iter f object_maps

(* on tile *)
(*  method foreach_tile f=
    self#get_tile_layer#foreach_map_entry f
*)
  method tile_layer_init tn t=
    let tl=self#get_tile_layer tn in
    for i=0 to self#get_rect#get_w-1 do
      for j=0 to self#get_rect#get_h-1 do
	let mt=randomize 2 in
	  tl#set_position i j (Some (t+mt)); 
      done;
    done;

(*  method position_blocking x y=
    if self#get_tile_layer#out_of_lay x y then true else false
*)
(*
    else
      if self#get_decor_map#is_object x y then
	let o=self#get_decor_map#get_object_by_position x y in
	  o#get_blocking
      else false
*)

(* actions *)

  method add_object_from_type mid=
    let m=self#get_object_map mid in
      m#add_object_from_type


  method add_object_named_from_type mid id t x y=
    let m=self#get_object_map mid in
      ignore(m#add_object_from_type (Some id) t x y)

  method copy_object mid=
    let m=self#get_object_map mid in
      m#copy_object

  method move_object mid=
    let m=self#get_object_map mid in
      m#move_object

  method delete_object mid=
    let m=self#get_object_map mid in
      m#delete_object

  method is_object mid=
    let m=self#get_object_map mid in
      m#is_object

  method get_object mid=
    let m=self#get_object_map mid in
      m#get_object

(* update layer *)

  method update()=
    self#foreach_object_map (fun i m->m#update());


(* persistance *)
  val mutable map_file=new file

  method objs_to_save=
    print_string ("GAME_MAP: save maps");print_newline();
    let a=Hashtbl.create 2 in
      self#foreach_object_map 
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
	let act=self#get_object_map n in
	  act#map_from_load v
    ) a;
    
  method private tile_to_save=
    let a=Hashtbl.create 2 in
      self#foreach_tile_layer 
      (
	
	fun n t->
	  print_string ("GAME_MAP: save "^n^" tile layer");print_newline();
	  Hashtbl.add a n t#get_lay
      );
      a

  method private tile_from_load al=
    Hashtbl.iter (
      fun n v->
	print_string ("GAME_MAP: load "^n^" tile layer");print_newline();  
	let t=self#get_tile_layer n in
	  t#set_lay v
    ) al;
      
  method save f=
    map_file#save f (self#get_rect#get_w,self#get_rect#get_h,self#tile_to_save,self#objs_to_save)
      
  method load f =
    print_string ("GAME_MAP: load maps");print_newline();
    let (mw,mh,tile_ar,obj_ar)=map_file#load f in
      self#resize mw mh;
      self#tile_from_load tile_ar;
      self#objs_from_load obj_ar;
      ()


  method lua_init()=
    lua#set_val (OLuaVal.String "add_object_from_type") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#add_object_named_from_type);
    lua#set_val (OLuaVal.String "delete_object") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) self#delete_object);
    lo#lua_init();

end;;

