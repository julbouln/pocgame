open Value_lua;;
open Value_val;;

open Core_stage;;
open Core_xml;;
open Core_main;;
open Core_event;;
open Core_interaction;;
open Core_video;;
open Core_stage;;
open Core_medias;;
open Core_graphic;;
open Core_font;;
open Binding;;

open Value_common;;

open Game_visual;;
open Game_map;;

open Core_mysql;;

(** World *)
class game_world=
object(self)
  inherit [game_map] generic_object_handler as super
  inherit lua_object as lo

  val mutable sql=new sql_xml

  method get_id="world"

  method add_map s o=
    ignore(self#add_object (Some s) o);
    ignore(o#lua_init()); 
    self#lua_parent_of s (o:>lua_object);

  val mutable init_map=(fun m->())

  method set_init_map f=init_map<-f

  method new_map_from_file s f=
    let nm=new game_map 0 0 in
      init_map nm;
      nm#load_from_file f;
      self#add_map s nm


  method set_object_state m=
    let map=self#get_object m in
      map#set_object_state

  method add_object_from_type m=
    let map=self#get_object m in
      map#add_object_from_type

  method delete_map_object m=
    let map=self#get_object m in
      map#delete_object

  method is_map_object m=
    let map=self#get_object m in
      map#is_object

  method update()=
    self#foreach_object(
      fun k v->
	v#update();
    );

  method lua_init()=
   lua#set_val (OLuaVal.String "foreach_map") 
     (OLuaVal.efunc (OLuaVal.value **->> OLuaVal.unit) 
	(fun f->
	   let g k v=
	     match f with
	       | OLuaVal.Function (s,f)->
		   f [OLuaVal.String k;OLuaVal.Table v#get_lua#to_table];()
	       | _ -> () in
	     self#foreach_object g
	));

   lua#set_val (OLuaVal.String "sql_connect") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) sql#connect);
   lua#set_val (OLuaVal.String "sql_disconnect") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) sql#disconnect);

   lua#set_val (OLuaVal.String "new_map_from_file") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) self#new_map_from_file);

    lo#lua_init();
end;;

(** Engine *)

class game_engine curs=
object(self)
  inherit stage curs as super

  val mutable interaction=new interaction_objects
  method set_interaction i=interaction<-i
  method get_interaction=interaction

  val mutable grille=
    new graphic_from_drawing "grille"
      (
      fun()->
	let dr=drawing_vault#new_drawing() in
	  dr#exec_op_create_from_list "rect" 
	    [
	      `Size(32,32);
	      `Color(200,200,200)
	    ];
	[|dr|]
    )

  val mutable map=new game_map 0 0
  method get_map=map


(** stage part : get & add & delete graphic in object id with graphic id *)
  method get_graphic id gid=
    let (mid,oid)=map#object_map_id id in
    let m=map#get_object_map mid in
    let oi=m#get_object oid in
    let ot=m#get_obj_type#get_object oi#get_name in
    let o=ot in
      (Some (o#get_graphic gid))

  method add_graphic id gid go=
    let (mid,oid)=map#object_map_id id in
    let m=map#get_object_map mid in
    let o=m#get_object oid in
      canvas#add_obj (go:>canvas_object);
      o#add_graphic gid go

  method delete_graphic id gid=
    let (mid,oid)=map#object_map_id id in
    let m=map#get_object_map mid in
    let o=m#get_object oid in
    let gr=o#get_graphic gid in
      canvas#del_obj (gr:>canvas_object);
      o#delete_graphic gid



  val mutable vrect=new game_visual 0 0
  method get_vrect=vrect

  method put_object_map_grille m=
    let vx=vrect#get_x and
	vy=vrect#get_y in
    map#foreach_object m (
      fun k obj->
	obj#around_object (map#get_object_map m)#out_of_lay (
	  fun x y->
	    grille#move (x*32-vx) (y*32-vy);
	    grille#put();
	)
    )


  initializer
    map#set_canvas (Some canvas);

  method on_load()=
    super#on_load();
    ignore(self#lua_init());

  method on_loop()=
    super#on_loop();
    map#update();
    interaction#foreach_object (
      fun ii i->
	i#on_loop()
    );

  method on_loop_graphic()=
    map#foreach_tile_layer (
      fun k t->
	t#put_map vrect;
    );
    canvas#refresh vrect#get_x vrect#get_y 32 32; 


  method ev_parser e=
    super#ev_parser e;

    interaction#foreach_object (
      fun ii i->
	i#ev_parser e
    )


  (* fun part *)
  method fun_init()=
    interaction#get_fnode#set_parent fnode;
    fnode#get_children#add_object (Some "interactions") interaction#get_fnode;

    map#fun_init();
    map#get_fnode#set_parent fnode;
    fnode#get_children#add_object (Some "map") map#get_fnode;

    vrect#fun_init();
    vrect#get_fnode#set_parent fnode;
    fnode#get_children#add_object (Some "visual") vrect#get_fnode;
    ()

  method lua_init()=
    lua#set_val (OLuaVal.String "put_object_map_grille") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (self#put_object_map_grille));

    ignore(interaction#lua_init());
    self#lua_parent_of "interactions" (interaction:>lua_object);

    ignore(map#lua_init());
    self#lua_parent_of "map" (map:>lua_object);

    ignore(vrect#lua_init());
    self#lua_parent_of "visual" (vrect:>lua_object);

    super#lua_init();
  
end;;


(* game world engine *)
class game_world_engine curs=
object(self)
  inherit stage curs as super

  val mutable interaction=new interaction_objects
  method set_interaction i=interaction<-i
  method get_interaction=interaction

  val mutable world=new game_world
  method get_world=world

  method on_load()=
    super#on_load();
    ignore(self#lua_init());


  method on_loop()=
    super#on_loop();
    world#update();
    interaction#foreach_object (
      fun ii i->
	i#on_loop()
    );

  method ev_parser e=
    super#ev_parser e;
    interaction#foreach_object (
      fun ii i->
	i#ev_parser e
    )

  (* fun part *)
  method fun_init()=
    interaction#get_fnode#set_parent fnode;
    fnode#get_children#add_object (Some "interactions") interaction#get_fnode;
    ()

  method lua_init()=
    ignore(interaction#lua_init());
    self#lua_parent_of "interactions" (interaction:>lua_object);

    ignore(world#lua_init());
    self#lua_parent_of "world" (world:>lua_object);

    super#lua_init();
  
end;;

