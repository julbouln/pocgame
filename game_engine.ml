
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
  method get_vrect=vrect

  method on_load()=
    map#init_type_from_xml file;


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
    map#lua_init();
    self#lua_parent_of "map" (map:>lua_object);
    super#lua_init();
  
end;;

open Interface;;
open Iface_object;;


let rec usleep sec = ignore (Unix.select [] [] [] sec);;



class game_engine_with_iface curs map_type_file iface_file englua=
object(self)
  val mutable engine=new game_engine curs map_type_file
  inherit iface_stage curs iface_file as iface



  method on_load()=

    engine#on_load();
    iface#on_load();

    ignore(self#lua_init());

    let engine_iobj=new iface_object video#get_w video#get_h in
      engine_iobj#set_layer (-1);
      engine_iobj#move 0 0;
      engine_iobj#show();
      engine_iobj#set_lua_script englua;
      engine_iobj#lua_parent_of "engine" (engine:>lua_object);
      engine_iobj#lua_init();


      iface#get_iface#iface_add_object "engine" engine_iobj;



	
  val mutable t1=0.
  val mutable t2=0.
				
					 
  method on_loop()=
    t1<-Unix.gettimeofday();
    engine#on_loop();
    iface#on_loop();
    curs#put();    
    video#flip();
    t2<-Unix.gettimeofday();

    
    if (t2 -. t1)<(1./. 30.) then
      usleep ((1./. 30.)  -. (t2 -. t1));     


  method ev_parser e=
    iface#ev_parser e;


  
    
  method lua_init()=
    engine#lua_init();
    self#lua_parent_of "engine" (engine:>lua_object);

    iface#lua_init();

end;;


open Oval;;
open Stage;;
open Core_xml;;

class xml_game_engine_with_iface_stage_parser=
object (self)
  inherit xml_stage_parser as super

  method parse_child k v=
    super#parse_child k v;

(** object initial init *)
  method init_object o=
    o#set_lua_script (lua);


  method get_val=
    let ofun()=
      let o=
	new game_engine_with_iface generic_cursor 
	  (string_of_val (args_parser#get_val#get_val (`String "map_type_file")))
	  (string_of_val (args_parser#get_val#get_val (`String "iface_file")))
	  (text_of_val (args_parser#get_val#get_val (`String "engine_iface_script")))
      in
	self#init_object (o:>stage);
	(o:>stage)	  
    in      
      (id,ofun)

end;;

let xml_engine_stages_parser()=
  let p=xml_iface_stages_parser() in
    p#parser_add "engine_stage_with_iface" (fun()->new xml_game_engine_with_iface_stage_parser);
    p;;

