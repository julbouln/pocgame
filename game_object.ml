
open Low;;
open Rect;;
open Video;;
open Medias;;

open Oxml;;
open Anim;;
open Action;;

open Otype;;

open Olua;;


type game_prop=
  | PropInt of int
  | PropFloat of float
  | PropString of string
  | PropBool of bool
  | PropList of game_prop list
  | PropLua of string * string
  | PropNil;;


exception Bad_game_prop of string

class game_properties=
object
  val mutable lo=new lua_object

  val mutable props=Hashtbl.create 2
  method add_prop (n:string) (p:game_prop)=Hashtbl.add props n p
  method set_prop n p=Hashtbl.replace props n p
  method del_prop n=Hashtbl.remove props n


  method lua_register (m:string) (interp:lua_interp)=
    lo#set_mod m;
    Hashtbl.iter (fun n v->
		    interp#parse (
		      match v with
			| PropFloat f->(m^"."^n^"="^string_of_float f)
			| PropInt i->(m^"."^n^"="^string_of_int i)
			| PropString s->(m^"."^n^"='"^s^"'")
			| PropBool b->(m^"."^n^"="^(if b then "true" else "false"))
			| PropLua (a,c) -> lo#add_function n a c;""
			| _ -> ""
		    );()
		 ) props;
    interp#parse_object lo;()

(*
  method from_db : ?
  method from_xml f : string -> unit
  method to_xml_message : unit ->string
  method to_lua : unit->string
*)

end;;

class xml_prop_parser=
object
  inherit xml_parser as super
  val mutable t=""
  val mutable nm=""
  val mutable v=""
  val mutable args=""

  method get_val=
    match t with
      | "int" -> (nm,PropInt (int_of_string v))
      | "float" -> (nm,PropFloat (float_of_string v))
      | "string" -> (nm,PropString v)
      | "lua" -> (nm,PropLua (args,v))
      | _ -> (nm,PropNil)

  method parse_attr k v=
    match k with
      | "name" -> nm<-v
      | "args" -> args<-v
      | _ -> ()

  method parse_child k v=()  
  method parse (n:xml_node)=
    super#parse n;
    t<-n#get_tag;
    v<-n#get_pcdata;
   
end;;

class xml_prop_list_parser=
object
  inherit [string * game_prop] xml_list_parser "" (fun()->new xml_prop_parser)
  method parse_child k v=
    match k with
      | _ -> let p=new xml_prop_parser in p#parse v;DynArray.add frms p#get_val
end;;



(*
 
 h=30x60x60 f = 108000 f
 m = 30x60 f = 1800 f
 s = 30 f
 f = f
*)

type time=
{
  h:int;
  m:int;
  s:int;
  f:int;
}


class game_time=
object(self)
  val mutable timers=Hashtbl.create 2
  method add_timer (t:time) (f:unit->unit)=
    Hashtbl.add timers (self#from_time t) f
  method del_timer (t:time)=
    Hashtbl.remove timers (self#from_time t)

  val mutable tasks=Hashtbl.create 2
  method add_task (t:time) (f:unit->unit)=
    Hashtbl.add tasks (self#from_time t) f
  method del_task (t:time)=
    Hashtbl.remove tasks (self#from_time t)

  val mutable frm=0
  val mutable cfrm=0

  val mutable run=false

  method start()=run<-true
  method stop()=run<-false

  method set_limit t=frm<-self#from_time t

  method get_cur_frame=cfrm

  method step()=
    if run then (
      Hashtbl.iter 
	(
	  fun tfr e->
	    if cfrm mod tfr=0 then e()
	) tasks;
      if Hashtbl.mem timers cfrm then
	let e=Hashtbl.find timers cfrm in e();
	  if cfrm<frm then
	    cfrm<-cfrm+1
	  else
	    cfrm<-0
    )

  method to_time fr=
    let h=fr/108000 and
	m=(fr mod 108000)/1800 and
	s=((fr mod 108000) mod 1800)/30 and
	f=(((fr mod 108000) mod 1800) mod 30) in
      {
	h=h;
	m=m;
	s=s;
	f=f;
      }
	
  method from_time t=
    (t.h*108000)+ (t.m * 1800) + (t.s*30) + t.f

end;;

(* more generic parent - without graphic *)
class game_obj (nm:string) (wi:int) (hi:int) (gwi:int) (ghi:int)=
object
 
    val mutable name=nm
    method get_name=name
    method set_name n=name<-n

    val mutable id=""
    method get_id=id
    method set_id i=id<-i

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


class ['a] game_obj_types (none_obj:'a)=
object
  inherit ['a] obj_types none_obj
end;;



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
  inherit game_action_object as action
 
(** time *)
  val mutable time=new game_time
  initializer
    time#start();
      time#set_limit       
	{
	  h=24;
	  m=0;
	  s=0;
	  f=0;
	}

  method act vx vy=
    action#act vx vy;
    time#step();

(** properties *)
  val mutable props=new game_properties
  method get_props=props
  method set_props p=props<-p
(** lua ? *)
  val mutable lua_code=""
  method set_lua l=lua_code<-l
  method get_lua=lua_code
       
  val mutable blocking=true;
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
	

  
(* DEPRECATED *)
    val mutable layer=0
    method set_layer l=layer<-l
    method get_layer=layer
(* /DEPRECATED *)


end;;

class game_graphics_container=
object
  val mutable graphs=DynArray.create()

  method add_graphic (gr:graphic_generic_object)=DynArray.add graphs gr

  method graphics_register reg=
    DynArray.iter (fun o->reg o) graphs
 
  method graphics_unregister unreg=
    DynArray.iter (fun o->unreg o) graphs
 

end;;


class game_graphic_object nm gwi ghi tilesfile mirror  is_shaded wi hi=
object(self)
  inherit game_generic_object nm wi hi gwi ghi as super

    val mutable graphic=new graphic_object gwi ghi tilesfile mirror is_shaded 

    method act vx vy=
      super#act vx vy;
      let cur=self#graphic#get_cur_tile in
	self#graphic#set_cur_tile (((self#graphic#get_tiles_size)/8)*direction + 
				     self#get_current_state#get_frame);

    method move x y=
      super#move x y;
      self#graphics_update()


    method graphics_register (reg:graphic_generic_object->unit)=
      reg graphic
    method graphics_update ()=
      self#graphic#move (self#get_pixel_x) (self#get_pixel_y);

    method graphic=graphic
    method get_graphic=graphic
    method set_graphic()=graphic<-new graphic_object gwi ghi tilesfile mirror is_shaded



    (* baricentre *)

    val mutable bcentre=(0,0)
    method get_bcentre_x=(fst bcentre)
    method get_bcentre_y=(snd bcentre)

(*    method around_object out_of_map (f:int->int->unit)=
      let rpos=self#graphic#get_rpos in
      let x1=rpos#get_x/32 and
	  y1=rpos#get_y/32 and
	  x2=rpos#get_w/32 and
	  y2=rpos#get_h/32 in

      for x=(self#get_case_x + x1 -self#get_bcentre_x/32 -1) to (self#get_case_x + x2 -self#get_bcentre_x/32 -1) do
	for y=(self#get_case_y + y1 -self#get_bcentre_y/32 -1) to (self#get_case_y + y2 -self#get_bcentre_y/32 -1) do
	  if out_of_map x y=false then f x y	    
	done;
      done;
*)
    method init_bcentre()=
      let rpos=self#graphic#get_rpos in
      let x1=rpos#get_x and
	y1=rpos#get_y and
	x2=rpos#get_w and
	y2=rpos#get_h in

	bcentre<-
	(
	  (x1+x2)/2,
	  (y1+y2)/2
	)

    method init_bcentre_with (graph:graphic_object)=
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

    method get_pixel_x=(rect#get_x*32) + prect#get_x - (fst bcentre) +16
    method get_pixel_y=(rect#get_y*32) + prect#get_y - (snd bcentre) +16

end;; 


class game_object nm gwi ghi tilesfile mirror is_shaded wi hi=
object (self)


  inherit game_graphic_object nm gwi ghi tilesfile mirror is_shaded wi hi
  initializer
    self#init_bcentre()

  method lua_register (interp:lua_interp)=
    interp#parse (id^"={}");
props#lua_register id interp

end;;



let none_obj=(new game_object "none" (!tile_w) (!tile_h) "none" false false 1 1 );;
let none_generic_obj=(new game_generic_object "none" (!tile_w) (!tile_h) 1 1 );;

(** game_object types *)
class game_object_types=
object
  inherit [game_object] obj_types none_obj
end;;


