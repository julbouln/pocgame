
open Low;;
open Rect;;
open Video;;
open Object;;

open Oxml;;
open Anim;;
open Action;;

open Obj_type;;
open Layer;;
open Obj_layer;;

class game_object nm gwi ghi tilesfile mirror is_shaded wi hi=
object (self)
  inherit obj nm wi hi

    val mutable bcentre=(0,0)
    method get_bcentre_x=(fst bcentre)
    method get_bcentre_y=(snd bcentre)

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

    initializer
      self#init_bcentre()

    val mutable sw=0
    val mutable sh=0
    method get_sw=sw
    method get_sh=sh

    val mutable layer=0
    method set_layer l=layer<-l
    method get_layer=layer

    val mutable prect=new rectangle 0 0 gwi ghi

    method update_prect()=
      let px=prect#get_x and
	py=prect#get_y and
	cx=rect#get_x and
	cy=rect#get_y in
      let xdif= 32-px and
	ydif= 32-py in

	if px<0 then (rect#set_position (cx-1) cy;prect#set_position (32+px) py);
	if px>32 then (rect#set_position (cx+1) cy;prect#set_position (px-32) py);
      	if py<0 then (rect#set_position cx (cy-1);prect#set_position px (32+py));
	if py>32 then (rect#set_position cx (cy+1);prect#set_position px (py-32));



    val mutable direction=0
    method get_direction=direction
    method turn dir=direction<-dir;


    val mutable graphic=new graphic_object gwi ghi tilesfile mirror is_shaded 
    method graphic=graphic

    val mutable need_put=true

    val mutable act_freq_cur=0;
    val mutable act_freq=4;
	
    method get_killed=0
    method set_killed (v:int)=()

    val mutable build_by=0
    method set_build_by v=build_by<-v
    method get_build_by=build_by

    val mutable blocking=true;
    method set_blocking b=blocking<-b
    method get_blocking=blocking
    method get_can_mulsel=false

    method get_graphic=graphic

    val mutable will_dead=false
    method i_will_dead=will_dead<-true;
    method will_i_dead=will_dead;


    method get_prect=prect


    method print_name=print_string name

    method play_snd (t:string) (vx:int) (vy:int)=()
    
    method resize w h=rect#set_size w h
    method move x y=self#set_case_position x y



    method dump()=      
      if name<>"none" then (
	print_string "UNIT DUMP : ";
	print_string "name : "; print_string name;print_string " - ";
	print_string "x : "; print_int (rect#get_x); print_string " - ";
	print_string "y : "; print_int (rect#get_y); print_string " - ";
	print_newline();
       );


(* ACTION NEW *)
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

(* /ACTION NEW *)


    method init_put()=need_put<-true;

    method put vx vy (tw:int) (th:int)=
      if need_put==true then (
	let cur=self#graphic#get_cur_tile in
	self#graphic#set_cur_tile (((self#graphic#get_tiles_size)/8)*direction + self#get_current_state#get_frame);
(*	  self#graphic#set_cur_tile ( self#get_current_state#get_frame); *)
	self#graphic#move (self#get_pixel_x - vx) (self#get_pixel_y - vy);
	self#graphic#put();
	need_put<-false;
       )


    method set_case_position x y=
      rect#set_position x y

    method get_pixel_x=rect#get_x *32 + 16 + prect#get_x - (fst bcentre) 
    method get_pixel_y=rect#get_y *32 + 16 + prect#get_y - (snd bcentre) 

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





    method put_shadow (vx:int) (vy:int) (tw:int) (th:int)=()
    method put_shaded (vx:int) (vy:int) (tw:int) (th:int)=()

    method set_life (l:int)=()
    method get_life=0
    method get_life_tot=0

    method get_cons_speed=0
    method get_c_cons_s=0
    method set_c_cons_s (v:int)=()



  end;;

(** FROM POCENGINE *)
(** graphical canvas *)
class canvas =
object(self)
  val mutable objs_list=RefList.empty()

  val mutable tile_list=RefList.empty()

  val mutable del_list=RefList.empty()

  method foreach_sorted f=
    self#foreach_obj f;

(** sort tiles to put from layer *)
  method sort_layer()=
    self#sort_obj  
       (fun ao bo ->
	    match ao#get_layer with
	      | x when x < bo#get_layer -> -1
	      | x when x = bo#get_layer -> 0
	      | x when x > bo#get_layer -> 1
	      | _ -> 0
      );


  method add_obj (o:game_object)=    
	RefList.add objs_list o;

  method del_dead ()=
    RefList.filter 
      ( fun o->
	  if o#will_i_dead=false then true else false	     	    
      )
      objs_list
 

  method sort_obj (f:game_object->game_object->int)=
    RefList.sort ~cmp:f objs_list;

  method foreach_obj (f:game_object->unit)=
      RefList.iter f objs_list


(** sort tiles to put from position *)

  method sort_position_NEW()=
    self#sort_obj 
      ( fun ao bo ->
	  let arect=ao#get_rect and
	    aprect=ao#get_prect and
	    arpos=ao#graphic#get_rpos and
	    brect=ao#get_rect and
	    bprect=ao#get_prect and
	    brpos=ao#graphic#get_rpos in
	  let get_x_p=
	    match ao#get_rect#get_x with
	      | x when (x*32 + ao#get_prect#get_x + arpos#get_w
			< bo#get_rect#get_x*32 + bo#get_prect#get_x + brpos#get_x) -> -1
	      | x when (x*32 + ao#get_prect#get_x + arpos#get_w 
			= bo#get_rect#get_x*32 + bo#get_prect#get_x + brpos#get_x) -> 0
	      | x when (x*32 + ao#get_prect#get_x + arpos#get_w 
			> bo#get_rect#get_x*32 + bo#get_prect#get_x + brpos#get_x) -> 1
	      | _ -> 0 and

	  get_y_p=
	    match ao#get_rect#get_y with
	      | y when (y*32 + ao#get_prect#get_y + arpos#get_h
			< bo#get_rect#get_y*32 + bo#get_prect#get_y + brpos#get_y) -> -1
	      | y when (y*32 + ao#get_prect#get_y + arpos#get_h 
			= bo#get_rect#get_y*32 + bo#get_prect#get_y + brpos#get_y) -> 0
	      | y when (y*32 + ao#get_prect#get_y + arpos#get_h 
			> bo#get_rect#get_y*32 + bo#get_prect#get_y + brpos#get_y) -> 1
	      | _ -> 0 in


	    get_x_p * get_y_p

    );

  method sort_position()=
    self#sort_obj 
      ( fun ao bo ->
	  match ao#get_rect with
	    | x when (x#get_y*32 + ao#get_prect#get_y < bo#get_rect#get_y*32 + bo#get_prect#get_y ) -> -1
	    | x when (x#get_y*32 + ao#get_prect#get_y = bo#get_rect#get_y*32 + bo#get_prect#get_y)  -> 0
	    | x when (x#get_y*32 + ao#get_prect#get_y > bo#get_rect#get_y*32 + bo#get_prect#get_y) -> 1
	    | x when (x#get_x*32 + ao#get_prect#get_x < bo#get_rect#get_x*32 + bo#get_prect#get_x) -> -1
	    | x when (x#get_x*32 + ao#get_prect#get_x = bo#get_rect#get_x*32 + bo#get_prect#get_x ) -> 0
	    | x when (x#get_x*32 + ao#get_prect#get_x > bo#get_rect#get_x*32 + bo#get_prect#get_x) -> 1
	    | _ -> 0
      );

  (** refresh the canvas. Refresh graphic part of each object *)
 method refresh check_fow vx vy tw th=
   self#del_dead();
   self#sort_layer();
   self#sort_position();
   self#foreach_obj (
      fun o->
	if o#get_rect#get_x*32>vx & o#get_rect#get_y*32>vy & vx<(o#get_rect#get_x*32+800) & vy<(o#get_rect#get_y*32+600) then 
	if (check_fow o#get_case_x o#get_case_y)==2 then
(*	  o#init_put(); *)
	  (

	    o#put_shadow vx vy tw th; 
	    o#put vx vy tw th;	
	  )
    )


end;;

let none_obj=(new game_object "none" (!tile_w) (!tile_h) "none" false false 1 1 );;

class game_object_types=
object
  inherit [game_object] game_obj_types none_obj
end;;

class game_object_layer wi hi max=
object(self)
  inherit [game_object] obj_layer none_obj wi hi max as super
  method init_put()=
    self#foreach_object (fun k o->
			o#init_put();
		     );

  method update_obj num=
    let obj=self#get_object num in
      obj#update_prect();
      
      super#update_obj num;

  method update_action()=
    self#foreach_object (fun k o->
			   o#act 0 0;
			   o#anim();
			)
end;;
