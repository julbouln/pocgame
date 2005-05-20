open Value_lua;;
open Value_val;;

open Core_stage;;
open Core_xml;;
open Core_main;;
open Core_event;;
open Core_video;;
open Core_stage;;
open Core_medias;;
open Core_graphic;;
open Core_font;;
open Binding;;

open Game_visual;;
open Game_map;;

(** Engine *)

class game_engine curs=
object(self)
  inherit stage curs as super

  val mutable interaction=new interaction_lua
  method set_interaction i=interaction<-i

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

(*
  val mutable canvas=new canvas
  method get_canvas=canvas
*)
  val mutable map=new game_map 0 0
  method get_map=map


(** stage part : get & add & delete graphic in object id with graphic id *)
  method get_graphic id gid=
    let (mid,oid)=map#object_map_id id in
    let m=map#get_object_map mid in
    let o=m#get_object oid in
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


  method on_loop_graphic()=
    map#foreach_tile_layer (
      fun k t->
	t#put_map vrect;
    );
    canvas#refresh vrect#get_x vrect#get_y 32 32; 


  method ev_parser e=
    super#ev_parser e;
    interaction#ev_parser e

  method lua_init()=
    lua#set_val (OLuaVal.String "put_object_map_grille") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (self#put_object_map_grille));

    ignore(interaction#lua_init());
    self#lua_parent_of "interaction" (interaction:>lua_object);

    ignore(map#lua_init());
    self#lua_parent_of "map" (map:>lua_object);

    ignore(vrect#lua_init());
    self#lua_parent_of "visual" (vrect:>lua_object);

    super#lua_init();
  
end;;

