
open Event;;
open Video;;
open Stage;;
open Medias;;

open Olua;;

open Game_visual;;
open Game_map;;

class game_engine curs file=
object(self)
  inherit stage curs as super

  val mutable canvas=new canvas
  method get_canvas=canvas

  val mutable map=new game_map 0 0
  method get_map=map

  method add_object_from_type mid id t x y=
    let n=map#add_object_from_type mid id t x y in
    let o=map#get_object mid n in
      o#graphics_register canvas#add_obj;
      n

  method add_object_named_from_type mid id t x y=
    let n=map#add_object_from_type mid (Some id) t x y in
    let o=map#get_object mid n in
      o#graphics_register canvas#add_obj;


  method delete_object mid id=
    let o=map#get_object mid id in
      o#graphics_unregister canvas#del_obj;
      map#delete_object mid id


  val mutable vrect=new game_visual 0 0


  method on_load()=
    ()

  method on_loop()=
    map#update();

    map#foreach_tile_layer (
      fun k t->
	t#put_map vrect;
    );
    canvas#refresh vrect#get_x vrect#get_y 32 32;


  method ev_parser e=
    (match e with
       | EventMouse em ->
	   (match em with
	      | MouseMotion(x,y) -> 
		  curs#move x y;
	      | MouseRelease(x,y,but) -> 
		  curs#set_state "normal";
	      | MouseClick(x,y,but) -> 
		  curs#set_state "clicked";
	      | _ -> ()
	   )
       | EventKeyboard ek->
	 (match ek with
	    | KeyboardPress (k,uk)-> ()
	    | _ -> ()
	 )
       | _ -> ()
    )

  method lua_init()=
    lua#set_val (OLuaVal.String "map_add_object_from_type") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#add_object_named_from_type);
    lua#set_val (OLuaVal.String "map_delete_object") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) self#delete_object);
    self#lua_parent_of "map" (map:>lua_object);
    super#lua_init();
  
end;;
