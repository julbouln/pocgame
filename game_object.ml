
open Rect;;
open Video;;
open Medias;;

open Drawing;;
open Generic;;
open Binding;;
open Graphic;;

open Oxml;;
open Anim;;
open Action;;
open Oval;;
open Otype;;

open Olua;;
open Timer;;

(*
<game_object_type name="decor">
 <graphics>
  <graphic_object id="main" type="graphic_from_file">
  <script>
   function self.on_update()
    obj=self.parent;
    self.move(obj.get_pixel_x,obj.get_pixel_y)
   end
  </script>
  </graphic_object>
 </graphics>
 <state_actions>
  <state_object id="idle">
   <action_object id="anim" type="action_anim"/>
    <args>
     <val_int name="refresh" value="2"/>
     <val_list name="frames">
      <val_int value="0"/>
     </val_list>
    </args>
    <script>
     function self.on_loop()
      obj=self.parent;
      obj.main.set_cur_drawing (self.get_frame());
     end
    </script>
   </action_object>
  </state_object>
 </state_actions>
</game_object_type>

<game_object name="montagne2" type="decor">
 <graphics>
  <graphic_object id="main" type="graphic_from_file">
   <args>
    <val_string name="filename" value="medias/misc/montagne2.png"/>
    <val_size name="size" w="500" h="500"/>
   </args>
  </graphic_object>
 </graphics>
 <args>
  <val_size name="case_size" w="15" h="5"/>
  <val_size name="pixel_size" w="500" h="500"/>
 </args>
 <properties>
  <val_string name="name" value="Montagne 2"/>
  <val_string name="editor_type" value="Montagne"/>
  <val_text name="desc">
   Grande Montagne
  </val_text>
 </properties> 
</game_object>
*)


(* more generic parent - without graphic *)
class game_obj (nm:string) (wi:int) (hi:int) (gwi:int) (ghi:int)=
object
  inherit generic_object 

    val mutable name=nm
    method get_name=name
    method set_name n=name<-n

    val mutable rect=new rectangle 0 0 wi hi
    method get_rect=rect

(* go in obj ? *)
    val mutable prect=new rectangle 0 0 gwi ghi
    method get_prect=prect

    method update_prect()=
      let px=prect#get_x and
	py=prect#get_y and
	cx=rect#get_x and
	cy=rect#get_y in
      let xdif= 32-px and
	ydif= 32-py in

	
	if px<0 then (
	  if py>=0 && py<32 then (
	    rect#set_position (cx-1) (cy);
	    prect#set_position (32+px) (py);
	  );
	  if py<0 then (
	    rect#set_position (cx-1) (cy-1);
	    prect#set_position (32+px) (32+py);
	  );
	  if py>=32 then (
	    rect#set_position (cx-1) (cy+1);
	    prect#set_position (32+px) (py-32);
	  )
	);


	if px>=0 && px<32 then (
	  if py>=0 && py<32 then (
	    rect#set_position cx (cy);
	    prect#set_position px (py);
	  );
	  if py<0 then (
	    rect#set_position (cx) (cy-1);
	    prect#set_position (px) (32+py);
	  );
	  if py>=32 then (
	    rect#set_position (cx) (cy+1);
	    prect#set_position (px) (py-32);
	  )
	);

	if px>=32 then (
	  if py>=0 && py<32 then (
	    rect#set_position (cx+1) (cy);
	    prect#set_position (px-32) (py);
	  );
	  if py<0 then (
	    rect#set_position (cx+1) (cy-1);
	    prect#set_position (px-32) (32+py);
	  );
	  if py>=32 then (
	    rect#set_position (cx+1) (cy+1);
	    prect#set_position (px-32) (py-32);
	  )
	);

(*
	if px>32 then (rect#set_position (cx+1) cy;prect#set_position (px-32) py);
      	if py<0 then (rect#set_position cx (cy-1);prect#set_position px (32+py));
	if py>32 then (rect#set_position cx (cy+1);prect#set_position px (py-32));
*)

    val mutable direction=0
    method get_direction=direction
    method turn dir=direction<-dir;

end;;



(* DEPRECATED *)
class game_action_object=
object(self)
    val mutable state_manager=new state_object_manager
    method set_state nm=
      state_manager#set_state nm 0

    method set_act a=state_manager#current_state#set_action a
    method get_current_state=state_manager#current_state
    method get_state=state_manager#get_cur_state

    method anim()=state_manager#current_state#anim();
    method act (vx:int) (vy:int)=
      state_manager#act();

    method act_start()=state_manager#current_state#start()
    method act_stop()=state_manager#current_state#stop()

end;;


class game_generic_object nm wi hi gwi ghi=
object(self)
  inherit game_obj nm wi hi gwi ghi
(*  inherit game_action_object as action *)
  val mutable states=new state_actions
  method get_states=states

  inherit lua_object as lo
 
(** time *)
  val mutable time=new timer
  initializer
    time#start();
    time#set_limit       
      {
	h=24;
	m=0;
	s=0;
	f=0;
      }

  method act()=
    states#act();
    time#step();

(** properties *)
  val mutable props=new val_ext_handler
  method get_props=props
  method set_props p=props<-p

       
  val mutable blocking=false;
  method set_blocking b=blocking<-b
  method get_blocking=blocking

  method resize w h=rect#set_size w h
  method move x y=self#set_case_position x y

  method set_case_position x y=
    rect#set_position x y
      
  method get_case_x=rect#get_x
  method get_case_y=rect#get_y
      
  method get_case_w=rect#get_w
  method get_case_h=rect#get_h
      
  method around_object out_of_map (f:int->int->unit)=
    for x=(self#get_case_x - self#get_case_w/2 ) to (self#get_case_x + self#get_case_w/2 ) do
      for y=(self#get_case_y - self#get_case_h/2 ) to (self#get_case_y + self#get_case_h/2 ) do
	    if out_of_map x y=false then f x y	    
	  done;
	done;


  method around_object1 out_of_map (f:int->int->unit)=
    let left=(self#get_case_x - self#get_case_w/2 -1)  
    and right=(self#get_case_x + self#get_case_w/2 +1)
    and top=(self#get_case_y - self#get_case_h/2 -1)
    and bottom=(self#get_case_y + self#get_case_h/2 +1) 
    in
	
      for x=(self#get_case_x - self#get_case_w/2 -1) to (self#get_case_x + self#get_case_w/2 +1) do
	for y=(self#get_case_y - self#get_case_h/2 -1) to (self#get_case_y + self#get_case_h/2 +1) do
	  if out_of_map x y=false	     
	  then
	    if (x<>left || y<>top)
	      && (x<>left || y<>bottom)
	      && (x<>right || y<>top)
	      && (x<>right || y<>bottom) then
		f x y	    
	done;
      done;

    method lua_init()=
      lua#set_val (OLuaVal.String "move") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#move);
      lua#set_val (OLuaVal.String "get_case_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_case_x));
      lua#set_val (OLuaVal.String "get_case_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_case_y));

      lua#set_val (OLuaVal.String "get_direction") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_direction));
      lua#set_val (OLuaVal.String "turn") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#turn);

      lo#lua_init();


      
end;;


class graphics_container=
object(self)
  inherit [graphic_object] generic_object_handler
  inherit lua_object as lo

  method add_graphic n gr=
    print_string ("GRAPHICS_CONTAINER : add graphic "^n);print_newline();
    self#add_object (Some n) gr;
    gr#lua_init();
    self#lua_parent_of n (gr:>lua_object)

  method graphics_update()=
    self#foreach_object (
      fun k v->
	ignore(v#get_lua#exec_val_fun (OLuaVal.String "on_update") [OLuaVal.Nil];)
    );
  
  method graphics_register reg=
    self#foreach_object (
      fun k o->
	reg (o:>canvas_object)
    );

  method graphics_unregister unreg=
    self#foreach_object (
      fun k o->
	unreg (o:>canvas_object)
    );
end;;

class game_object nm tilesfile gwi ghi wi hi=
object(self)
  inherit game_generic_object nm wi hi gwi ghi as super

  val mutable graphics=new graphics_container
  method get_graphics=graphics
(*
  val mutable graphic=new graphic_from_file tilesfile gwi ghi
*)
(*  initializer
    self#init_bcentre()
*)
  method act()=
    super#act();
(*    let cur=self#graphic#get_cur_drawing in
      self#graphic#set_cur_drawing (((self#graphic#get_drawings_size)/8)*direction + 
				   self#get_current_state#get_frame);
*)    
  method move x y=
    super#move x y;
    self#graphics_update()
      
  method graphics_register (reg:canvas_object->unit)=
    graphics#graphics_register reg;
(*    reg (graphic:>canvas_object) *)
  method graphics_unregister (unreg:canvas_object->unit)=
    graphics#graphics_unregister unreg;
(*    unreg (graphic:>canvas_object) *)
  method graphics_update ()=
    graphics#graphics_update()
(*    self#graphic#move (self#get_pixel_x) (self#get_pixel_y); *)
(*
  method graphic=graphic
  method get_graphic=graphic
  method set_graphic()=graphic<-new graphic_from_file tilesfile gwi ghi
*)

  (* baricentre *)
    
  val mutable bcentre=(0,0)
  method get_bcentre_x=(fst bcentre)
  method get_bcentre_y=(snd bcentre)

  method init_bcentre gr_id=
(*    let rpos=self#graphic#get_rpos in *)
(*    let rpos=get_rpos (drawing_vault#get_cache_simple tilesfile) in *)
(*    let dr=(drawing_vault#get_cache_simple tilesfile) in *)
    let dr=(graphics#get_object gr_id)#get_drawing 0 in
    let lv=list_of_val (
	dr#exec_op_read_from_list "get_rpos" [`Color(255,36,196)]
    ) in
      

    let (x1,y1)=position_of_val (List.nth lv 0) and
	(x2,y2)=position_of_val (List.nth lv 1) in
      
      bcentre<-
	(
	  (x1+x2)/2,
	  (y1+y2)/2
	)
(*	
  method init_bcentre_with (graph:graphic_obect)=
    let rpos=graph#get_rpos in
    let x1=rpos#get_x and
	y1=rpos#get_y and
	x2=rpos#get_w and
	y2=rpos#get_h in
      
      bcentre<-
	(
	  (x1+x2)/2,
	  (y1+y2)/2
	)
*)
  method get_pixel_x=(rect#get_x*32) + prect#get_x - (fst bcentre) + 16
  method get_pixel_y=(rect#get_y*32) + prect#get_y - (snd bcentre) + 16


  method lua_init()=
    lua#set_val (OLuaVal.String "get_pixel_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_pixel_x));
    lua#set_val (OLuaVal.String "get_pixel_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_pixel_y));
    lua#set_val (OLuaVal.String "init_bcentre") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (self#init_bcentre));
    lua#set_val (OLuaVal.String "properties") (OLuaVal.Table props#to_lua#to_table);

    graphics#lua_init();
    self#lua_parent_of "graphics" (graphics:>lua_object);
    states#lua_init();
    self#lua_parent_of "states" (states:>lua_object);
    super#lua_init();

end;; 

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
  inherit [game_object] xml_object_parser (fun()->new game_object "" "" 0 0 0 0) as super
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
	let args=args_parser#get_val in
	let (gw,gh)=size_of_val (args#get_val (`String "pixel_size")) and
	    (w,h)=size_of_val (args#get_val (`String "case_size")) in
	  new game_object id "" gw gh w h
      in
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

(** types *)

(* deprecated *)
class ['a] game_obj_types=
object
  inherit ['a] obj_types
end;;


(** game_object types *)
class game_object_types=
object(self)
  inherit [game_object] obj_types

  method init_from_xml f=
    init_game_object_types_from_xml f self#add_object_type

end;;


