
open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_drawing;;
open Core_generic;;
open Core_graphic;;
open Core_anim;;
open Core_action;;
open Core_type;;
open Core_timer;;

open Binding;;

open Oxml;;
open Oval;;
open Olua;;


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
class game_obj=
object
  inherit generic_object 

    val mutable name=""
    method get_name=name
    method set_name n=name<-n

    val mutable rect=new rectangle 0 0 0 0
    method get_rect=rect

(* go in obj ? *)
    val mutable prect=new rectangle 0 0 0 0
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

    method scroll p=
      let px=prect#get_x and
	  py=prect#get_y in 
	match direction with
	  | 0 -> prect#set_position px (py-p);
	  | 2 -> prect#set_position (px + p) py;
	  | 4 -> prect#set_position px (py+p); 
	  | 6 -> prect#set_position (px-p) py;
	  | _ -> ();
	      
    method next_position()=
      let x=rect#get_x and
	  y=rect#get_y in
      match direction with
	  | 0 -> (x,y-1);
	  | 2 -> (x+1,y);
	  | 4 -> (x,y+1); 
	  | 6 -> (x-1,y);
	  | _ -> (x,y);


end;;




class game_generic_object=
object(self)
  inherit game_obj
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

      lua#set_val (OLuaVal.String "get_prect_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->prect#get_x));
      lua#set_val (OLuaVal.String "get_prect_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->prect#get_y));

      lua#set_val (OLuaVal.String "set_prect_position") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (prect#set_position));


      lua#set_val (OLuaVal.String "get_type") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_name));

      lua#set_val (OLuaVal.String "get_id") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_id));

      lua#set_val (OLuaVal.String "scroll") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#scroll);
      lua#set_val (OLuaVal.String "next_position") 
	(OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.value) 
	   (fun()->
	     let np=self#next_position() in
	       lua_of_val_ext (`Position np)
	   )
	);

      lo#lua_init();


      
end;;


class graphics_container=
object(self)
  inherit [graphic_object] generic_object_handler
  inherit lua_object as lo

  method add_graphic n gr=
(*    print_string ("GRAPHICS_CONTAINER : add graphic "^n);print_newline(); *)
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

class game_object=
object(self)
  inherit game_generic_object as super

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

  method turn d=
    super#turn d;
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

    lua#set_val (OLuaVal.String "turn") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#turn);
    lua#set_val (OLuaVal.String "get_direction") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_direction));



    lua#set_val (OLuaVal.String "graphics_update") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (self#graphics_update));

    graphics#lua_init();
    self#lua_parent_of "graphics" (graphics:>lua_object);
    states#lua_init();
    self#lua_parent_of "states" (states:>lua_object);

    super#lua_init();

end;; 


(** types *)

(* deprecated *)
class ['a] game_obj_types=
object
  inherit ['a] obj_types
end;;





