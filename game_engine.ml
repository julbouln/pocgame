open Core_main;;
open Core_event;;
open Core_video;;
open Core_stage;;
open Core_medias;;
open Core_graphic;;
open Binding;;

open Olua;;

open Game_visual;;
open Game_map;;

class game_engine curs file=
object(self)
  inherit stage curs as super


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


  val mutable canvas=new canvas
  method get_canvas=canvas

  val mutable map=new game_map 0 0
  method get_map=map


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




  method on_load()=
    map#set_canvas (Some canvas);
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

    lua#set_val (OLuaVal.String "put_object_map_grille") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (self#put_object_map_grille));

    ignore(map#lua_init());
    self#lua_parent_of "map" (map:>lua_object);

    ignore(vrect#lua_init());
    self#lua_parent_of "visual" (vrect:>lua_object);

    super#lua_init();
  
end;;

open Interface;;
open Iface_object;;


let rec usleep sec = ignore (Unix.select [] [] [] sec);;



class game_engine_with_iface curs map_type_file iface_file englua=
object(self)
  val mutable engine=new game_engine curs map_type_file
  inherit iface_stage curs iface_file as iface

  val mutable engine_iobj=new iface_object video#get_w video#get_h 

  method on_load()=

    engine#on_load();
    iface#on_load();

    ignore(self#lua_init());


      engine_iobj#set_layer (-1);
      engine_iobj#move 0 0;
      engine_iobj#show();
      engine_iobj#set_grab_focus true;
      engine_iobj#set_lua_script englua;
      engine_iobj#lua_parent_of "engine" (engine:>lua_object);
      ignore(engine_iobj#lua_init());

      iface#get_iface#iface_add_object "engine" engine_iobj;
      ignore(engine_iobj#get_lua#exec_val_fun (OLuaVal.String "on_load") [OLuaVal.Nil]);


	
  val mutable t1=0.
  val mutable t2=0.

  val mutable ffps=float main#get_fps
				
					 
  method on_loop()=
    t1<-Unix.gettimeofday();
    video#blank();
    engine#on_loop();

      ignore(engine_iobj#get_lua#exec_val_fun (OLuaVal.String "on_loop") [OLuaVal.Nil]);
    iface#on_loop();
    curs#put();    

    t2<-Unix.gettimeofday();

    
    
    if (t2 -. t1)<(1./. ffps) then
      usleep ((1./. ffps)  -. (t2 -. t1));     

    video#flip();



  method ev_parser e=
    iface#ev_parser e;


  
    
  method lua_init()=
    ignore(engine#lua_init());
    self#lua_parent_of "engine" (engine:>lua_object);

    iface#lua_init();


end;;


open Oval;;
open Core_stage;;
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
	  (text_of_val (args_parser#get_val#get_val (`String "script")))
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

