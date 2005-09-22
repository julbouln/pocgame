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

open Value_common;;
open Value_object;;
open Value_lua;;
open Value_xml;;
open Value_xinclude;;
open Value_val;;

open Core_fun;;
open Core_val;;
open Core_medias;;
open Core_graphic;;
open Core_rect;;
open Core_file;;
open Core_action;;
open Core_xml;;
open Core_type;;

open Game_object;;
open Game_object_layer;;
open Game_tile_layer;;
open Game_pathfinding;;

(** Map *)

(** game_object types *)
class game_object_types=
object(self)
  inherit [game_object] obj_types
end;;

class game_object_map wi hi=
object(self)
  inherit generic_object
  inherit lua_object as lo
  inherit xml_object
  inherit [game_object] game_obj_layer wi hi as super

  (* fun part *)
  val mutable fnode=new core_fun_node
  method get_fnode=fnode

  method get_object_id_at_position x y=
    self#get_position x y
      
  method functionize : functionizer=
    `GameObjectMapFun (self:>game_object_map_fun)

  method fun_init()=
    fnode#set_fun self#functionize;

  val mutable canvas=None
  method set_canvas (c:canvas option)=canvas<-c

  method is_obj_with_type t=
    let r=ref false in
      self#foreach_object
	( fun i o->
	    if o#get_name=t then r:=true
	);
      !r

  method clear()=
    super#clear();
    (match canvas with 
       | Some cvas->cvas#clear();
       | None -> ());

	
  val mutable obj_type=new game_object_types
  method get_obj_type=obj_type
    
  method add_object_type nm (t:unit->'a)=
    obj_type#add_object_type nm t
  method get_object_from_type nm=
    obj_type#get_object_type nm


  method add_object_to_canvas o=
    (match canvas with 
       | Some cvas->o#graphics_register cvas#add_obj;
       | None -> ());
  method del_object_from_canvas o=
    (match canvas with 
       | Some cvas->o#graphics_unregister cvas#del_obj;
       | None -> ());

  (** general *)
  method add_object_at (id:string option) (o:'a) (x:int) (y:int)=    
    self#add_object_to_canvas o;
    let n=self#add_object id o in
      o#fun_init();
      o#get_fnode#set_parent fnode;
      fnode#get_children#add_object (Some n) o#get_fnode;
      ignore(o#lua_init());
      self#lua_parent_of n (o:>lua_object);
      o#move x y;
      n
	  
  method update()=
    self#update_obj_all();
    self#update_action(); 


  method add_object_from_type id t x y=
    let o=self#get_object_from_type t in
      self#add_object_at id o x y 

  method delete_object id=
    let o=self#get_object id in
      self#del_object_from_canvas o;
      lo#get_lua#del_val (OLuaVal.String id) ;
      super#delete_object id;

  method copy_object cid id=
    let o=self#get_object id in
    let no=(self#get_object_from_type o#get_name) in
    self#add_object_at cid no o#get_rect#get_x o#get_rect#get_y

  method xml_to_init()=
    xml<-new xml_node;
    let a=DynArray.create() in
      self#foreach_object (
	fun k o->
	  let e=new xml_node in
	    e#of_list [
	      Tag "game_object";
	      Attribute ("id",k);
	      Attribute ("type",o#get_name);
	    ];
	    (* args *)
	    let vh=new val_ext_handler in
	      vh#set_id "args";
	      vh#set_val (`String "position") (`Position (o#get_rect#get_x,o#get_rect#get_y));
	      vh#set_val (`String "pixel_position") (`Position (o#get_prect#get_x,o#get_prect#get_y));
	      
	      
	      e#add_child vh#to_xml;
	      (* properties *)
	      o#reinit_caml_props();
	      let pr=(o#get_props) in
		e#add_child pr#to_xml;
    
	      DynArray.add a e#to_node;
      );
      
      xml#of_list (DynArray.to_list a);
    xml#set_tag "game_object_map";
    xml#add_attrib ("id",self#get_id);

  method xml_of_init()=
    List.iter (
      fun c->	
	let args=new val_ext_handler and
	    props=new val_ext_handler in
	  List.iter (
	    fun cc->
	      match (cc#tag) with
		| "args" -> 
		    args#from_xml cc
		| "properties" -> 
		    props#from_xml cc 
		| _ ->()
	  ) c#children;
		let (x,y)=position_of_val (args#get_val (`String "position")) in

		  
		let oid=(c#attrib "id") and
		    otype=(c#attrib "type") in

		  if self#is_object oid then (
		    let o=self#get_object oid in
		      o#move x y;
		      let (px,py)=position_of_val (args#get_val (`String "pixel_position")) in
		      o#jump px py;
		  )		    
		  else
		    ignore(self#add_object_from_type (Some oid) otype x y);
		  
		  let o=self#get_object oid in
		    o#get_props#flatten props; 
		    o#reinit_lua_props();
(*		    o#set_props props; *)
    ) xml#children;



 method lua_init()=
   lua#set_val (OLuaVal.String "clear") 
     (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#clear); 

   lua#set_val (OLuaVal.String "add_object_from_type") 
     (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.string) 
	(fun t x y->self#add_object_from_type None t x y));
   lua#set_val (OLuaVal.String "add_object_named_from_type") 
     (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) 
	(fun n t x y->ignore(self#add_object_from_type (Some n) t x y)));
   
   lua#set_val (OLuaVal.String "delete_object") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#delete_object);

   lua#set_val (OLuaVal.String "foreach_object") 
     (OLuaVal.efunc (OLuaVal.value **->> OLuaVal.unit) 
	(fun f->
	   let g k v=
	     match f with
	       | OLuaVal.Function (s,f)->
		   f [OLuaVal.String k;OLuaVal.Table v#get_lua#to_table];()
	       | _ -> () in
	     self#foreach_object g
	));

    lua#set_val (OLuaVal.String "get_object_id_at_position") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.value) 
	 (fun x y->
	    let n=self#get_position x y in
	      match n with 
		| Some i -> OLuaVal.String i
		| None -> OLuaVal.Nil
	 )
      );

    lua#set_val (OLuaVal.String "get_object_at_position") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.value) 
	 (fun x y->
	    let n=self#get_position x y in
	      match n with 
		| Some i -> (lua#get_val (OLuaVal.String i))
		| None -> OLuaVal.Nil
	 )
      );

    obj_type#lua_init();
    self#lua_parent_of "types" (obj_type:>lua_object); 

    lo#lua_init();

end;;



class game_tile_layer_handler=
object
  inherit [game_generic_tile_layer] generic_object_handler
end;;

class game_object_map_handler=
object
  inherit [game_object_map] generic_object_handler
end;;



(* game map *)

class game_map w h=
object(self)
  inherit generic_object as go
  inherit lua_object as lo
  inherit xml_object

  initializer
    go#set_id "map"

  val mutable actions=new state_object

  val mutable canvas=None
  method set_canvas (c:canvas option)=canvas<-c

  val mutable rect=new rectangle 0 0 w h
  method get_rect=rect



  (** PATHFINDING *)
  val mutable path=new pathfinding_map w h

  method pathfinding_init()=
    path#init_empty rect#get_w rect#get_h;
    self#foreach_map_object (fun oid o->
			       if o#get_blocking=true then (
				 path#set_position (o#get_case_x) (o#get_case_y) false
							      
			       )
			    );
    path#init();

(* fun part *)
  val mutable fnode=new core_fun_node
  method get_fnode=fnode

  method path_calc cur dest=
    path#path_calc cur dest

  method functionize : functionizer=
    `GameMapFun (self:>game_map_fun)

  method fun_init()=
    fnode#set_fun self#functionize;



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

  (* tile layers *)
  val mutable tile_layers=new game_tile_layer_handler

  method add_tile_layer (s:string) (o:game_generic_tile_layer)=
(*    print_string "add tile layer";print_newline(); *)
    ignore(tile_layers#add_object (Some s) o);
    ignore(o#lua_init()); 
    self#lua_parent_of s (o:>lua_object);

  method get_tile_layer (s:string)=
    tile_layers#get_object s

  method foreach_tile_layer f=
    tile_layers#foreach_object f

  method tile_layer_init tn t=
    let tl=self#get_tile_layer tn in
    for i=0 to self#get_rect#get_w-1 do
      for j=0 to self#get_rect#get_h-1 do
	let mt=randomize 2 in
	  tl#set_position i j (Some (t+mt)); 
      done;
    done;


  (* object maps *)
  val mutable object_maps=new game_object_map_handler
    
  method add_object_map (s:string) (o:game_object_map)=
(*    print_string "add object map";print_newline(); *)
    o#set_canvas canvas;

    o#fun_init();
    o#get_fnode#set_parent fnode;
    fnode#get_children#add_object (Some s) o#get_fnode;

    ignore(object_maps#add_object (Some s) o);
    ignore(o#lua_init()); 
    self#lua_parent_of s (o:>lua_object);
    
  method get_object_map (s:string)=
    object_maps#get_object s

  method foreach_object_map f=
    object_maps#foreach_object f


  method is_position_blocking x y=
    if rect#is_position x y then
      (
	let r=ref false in
	  self#foreach_object_map (
	    fun mid m->
	      if m#is_object_at_position x y then
	      let o=m#get_object_at_position x y in
		r:=!r || o#get_blocking
	  );
	  !r
      )
    else
      true
(*    if self#get_tile_layer#out_of_lay x y then true else false
*)
(*
    else
      if self#get_decor_map#is_object x y then
	let o=self#get_decor_map#get_object_by_position x y in
	  o#get_blocking
      else false
*)

(* objects *)

(** get "mapid.objid" in (mapid,objid) *)
  method object_map_id id=
    let lid=Str.split (Str.regexp "#") id in
    let mid=List.nth lid 0 and
	oid=List.nth lid 1 in
(*      print_string (mid^"."^oid);print_newline(); *)
      (mid,oid)

(** comp *)
  method object_map_get_map_id id=
    let (mid,oid)=self#object_map_id id in
      mid
  method object_map_get_object_id id=
    let (mid,oid)=self#object_map_id id in
      oid

  method add_object_named_from_type_comp id t x y=
    let (mid,oid)=self#object_map_id id in
      self#add_object_named_from_type mid oid t x y

  method get_object_id_at_position_comp x y=
    let r=ref (None:string option) in
      self#foreach_object_map (
	fun mid m->
	  let rr=m#get_object_id_at_position x y in
	    match rr with
	      | Some oid->r:=Some (mid^"#"^oid)
	      | None -> ()
      );

      !r

  method delete_object_comp id=
    let (mid,oid)=self#object_map_id id in
      self#delete_object mid oid

(** obj map methods *)
  method add_object_from_type mid id t x y=
    let m=self#get_object_map mid in
    let n=m#add_object_from_type id t x y in
      n

  method add_object_named_from_type mid id t x y=
    let m=self#get_object_map mid in
      ignore(m#add_object_from_type (Some id) t x y);
	
  method is_object mid=
    let m=self#get_object_map mid in
      m#is_object

  method copy_object mid=
    let m=self#get_object_map mid in
      m#copy_object

  method move_object mid=
    let m=self#get_object_map mid in
      m#move_object

  method set_object_state mid oid st_n st_v=
    let m=self#get_object_map mid in
    let o=m#get_object oid in
      o#get_states#set_state st_n st_v

  method delete_object mid id=
    let m=self#get_object_map mid in
      m#delete_object id

  method is_object mid=
    let m=self#get_object_map mid in
      m#is_object

  method get_object mid=
    let m=self#get_object_map mid in
      m#get_object

  method get_object_id_at_position mid x y=
    let m=self#get_object_map mid in
    m#get_position x y

  method foreach_object mid f=
    let m=self#get_object_map mid in
      m#foreach_object f

  method foreach_map_object f=
    self#foreach_object_map 
      (fun mid m->
	   m#foreach_object f
      );
  method move_object_to_map mid dmid oid=
    let m=self#get_object_map mid and
	dm=self#get_object_map dmid in
    let o=m#get_object oid in
      m#delete_object oid;
      let n=dm#add_object_at (Some oid) o o#get_rect#get_x o#get_rect#get_y in
	()
      

(* update layer *)

  method update()=
    self#foreach_object_map (fun i m->m#update());
    actions#loop();

  method xml_to_init()=
    xml<-new xml_node;
    xml#of_list [
      Tag "game_map";
      Attribute ("w",string_of_int self#get_rect#get_w);
      Attribute ("h",string_of_int self#get_rect#get_h);
    ];

(* object maps *)    
    let mapn=new xml_node in
      mapn#set_tag "game_object_maps";
      
      self#foreach_object_map(
	fun k m->
	  m#xml_to_init();
	  mapn#add_child m#get_xml
      );
      xml#add_child mapn;

(* tile layers *)
    let mapt=new xml_node in
      mapt#set_tag "game_tile_layers";
      
      self#foreach_tile_layer(
	fun k m->
	  m#xml_to_init();
	  mapt#add_child m#get_xml
      );
      xml#add_child mapt;

      



  method xml_of_init()=
    let w=int_of_string(xml#attrib "w") and
	h=int_of_string(xml#attrib "h") in
      self#resize w h;
      
      List.iter(
	fun c->
	  match (c#tag) with
	    | "game_object_maps" ->
		List.iter (
		  fun oc->
		    let om=self#get_object_map (oc#attrib "id") in
		      om#set_xml oc;
		      om#xml_of_init();
		) (c#children);
	    | "game_tile_layers" ->
		List.iter (
		  fun oc->
		    let ot=self#get_tile_layer (oc#attrib "id") in
		      ot#set_xml oc;
		      ot#xml_of_init();
		) (c#children);
		
	    | "actions" ->
		let p=(Global.get xml_default_actions_parser)() in
		  p#parse c;
		  p#init_simple (actions#add_action);
	    | _ -> ()
      ) xml#children;



    
  method to_node=
    self#xml_to_init();
    xml

  method of_node n=
    xml#of_node n;
    self#xml_of_init();

  method save_to_file f=
    let fo=open_out f in
      self#xml_to_init();
      output_string fo (xml#to_string); 
      close_out fo;

  method load_from_file f=
    xml#of_file f;
    self#xml_of_init();

    path#init_empty rect#get_w rect#get_h;
    self#foreach_map_object (fun oid o->
			       if o#get_blocking=true then (
				 path#set_position (o#get_case_x) (o#get_case_y) false
				   
			       )
			    );
      path#init();


  method clear()=
    self#foreach_object_map(
      fun k om->
	  om#clear()
    );

  method lua_init()=
   lua#set_val (OLuaVal.String "clear") 
     (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#clear); 

(* compatible with map object funcs *)
    lua#set_val (OLuaVal.String "add_object_named_from_type") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#add_object_named_from_type_comp);

    lua#set_val (OLuaVal.String "object_get_map_id") (OLuaVal.efunc (OLuaVal.string **->>  OLuaVal.string) self#object_map_get_map_id);
    lua#set_val (OLuaVal.String "object_get_object_id") (OLuaVal.efunc (OLuaVal.string **->>  OLuaVal.string) self#object_map_get_object_id);

    lua#set_val (OLuaVal.String "delete_object") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#delete_object_comp);
    lua#set_val (OLuaVal.String "get_object_id_at_position") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.value) 
	 (fun x y->
	    let n=self#get_object_id_at_position_comp x y in
	      match n with 
		| Some i -> OLuaVal.String i
		| None -> OLuaVal.Nil
	 )
      );

(* DEPRECATED use mapname.func instead *)
(*
    lua#set_val (OLuaVal.String "add_object_from_type") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.string) (fun m t x y->self#add_object_from_type m None t x y));
*)
(*    lua#set_val (OLuaVal.String "add_object_named_from_type") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#add_object_named_from_type);
*)


(*
    lua#set_val (OLuaVal.String "delete_object") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) self#delete_object);
    lua#set_val (OLuaVal.String "get_object_id_at_position") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.value) 
	 (fun m x y->
	    let n=self#get_object_id_at_position m x y in
	      match n with 
		| Some i -> OLuaVal.String i
		| None -> OLuaVal.Nil
	 )
      );
*)
(* /DEPRECATED *)


   lua#set_val (OLuaVal.String "foreach_object_map") 
     (OLuaVal.efunc (OLuaVal.value **->> OLuaVal.unit) 
	(fun f->
	   let g k v=
	     match f with
	       | OLuaVal.Function (s,f)->
		   f [OLuaVal.String k;OLuaVal.Table v#get_lua#to_table];()
	       | _ -> () in
	     self#foreach_object_map g
	));

    lua#set_val (OLuaVal.String "move_object_to_map") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) self#move_object_to_map);

    lua#set_val (OLuaVal.String "is_position_blocking") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.bool) 
	 self#is_position_blocking
      );


    lua#set_val (OLuaVal.String "init_tile_layer") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.int **->> OLuaVal.unit) self#tile_layer_init);
    lua#set_val (OLuaVal.String "resize") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#resize);

    lua#set_val (OLuaVal.String "load_from_file") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#load_from_file);
    lua#set_val (OLuaVal.String "save_to_file") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#save_to_file);


    lua#set_val (OLuaVal.String "pathfinding_init") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#pathfinding_init);


    actions#set_id "actions";
    ignore(actions#lua_init());
    self#lua_parent_of "actions" (actions:>lua_object);

    lo#lua_init();

end;;

