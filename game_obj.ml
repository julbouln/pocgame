
open Low;;
open Rect;;
open Video;;
open Object;;

open Oxml;;
open Anim;;
open Action;;

open Otype;;
open Obj_type;;
open Layer;;
open Obj_layer;;

class game_action_object=
object(self)

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

end;;


class game_generic_object nm wi hi gwi ghi=
object(self)
  inherit obj nm wi hi gwi ghi
  inherit game_action_object

  val mutable lua_code=""
  method set_lua l=lua_code<-l
  method get_lua=lua_code


  
    val mutable sw=0
    val mutable sh=0
    method get_sw=sw
    method get_sh=sh

(* DEPRECATED *)
    val mutable layer=0
    method set_layer l=layer<-l
    method get_layer=layer


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


    val mutable will_dead=false
    method i_will_dead=will_dead<-true;
    method will_i_dead=will_dead;





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
	


    method set_life (l:int)=()
    method get_life=0
    method get_life_tot=0

    method get_cons_speed=0
    method get_c_cons_s=0
    method set_c_cons_s (v:int)=()
end;;

class game_graphic_object nm gwi ghi tilesfile mirror  is_shaded wi hi=
object(self)
  inherit game_generic_object nm wi hi gwi ghi as super

    val mutable graphic=(*new g_object "none"*)
new graphic_object gwi ghi tilesfile mirror is_shaded 


    method act vx vy=
      super#act vx vy;
      let cur=self#graphic#get_cur_tile in
	self#graphic#set_cur_tile (((self#graphic#get_tiles_size)/8)*direction + 
				     self#get_current_state#get_frame);

    method move x y=
      super#move x y;
      self#graphic#move (self#get_pixel_x) (self#get_pixel_y);


    method graphics_register (reg:graphic_generic_object->unit)=
      reg graphic

    method graphic=graphic

    method get_graphic=graphic

    method set_graphic()=graphic<-new graphic_object gwi ghi tilesfile mirror is_shaded

    val mutable need_put=true



(* GRAPH *)
    method init_put()=need_put<-true;

(* GRAPH *)
    method put vx vy (tw:int) (th:int)=
      if need_put==true then (

	let cur=self#graphic#get_cur_tile in
	self#graphic#set_cur_tile (((self#graphic#get_tiles_size)/8)*direction + 
				   self#get_current_state#get_frame);
	  (*	  self#graphic#set_cur_tile ( self#get_current_state#get_frame); *)
	  self#graphic#move (self#get_pixel_x - vx) (self#get_pixel_y - vy);
	  self#graphic#put();
	  need_put<-false;
      )


    val mutable bcentre=(0,0)
    method get_bcentre_x=(fst bcentre)
    method get_bcentre_y=(snd bcentre)

    method around_object out_of_map (f:int->int->unit)=
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

    method get_pixel_x=rect#get_x *32 + 16 + prect#get_x - (fst bcentre) 
    method get_pixel_y=rect#get_y *32 + 16 + prect#get_y - (snd bcentre) 

(* shadow *)    
(*    val mutable shadow=new graphic_object 34 11 "medias/misc/shadow.png" false false
*)
    method put_shadow (vx:int) (vy:int) (tw:int) (th:int)=
(*      shadow#set_cur_tile 0;
      shadow#move (self#get_pixel_x - vx + self#graphic#get_rect#get_w/8) (self#get_pixel_y - vy + (4*self#graphic#get_rect#get_h)/5 + 4);
      shadow#put();
*)()
    method put_shaded (vx:int) (vy:int) (tw:int) (th:int)=()


end;; 


class game_object nm gwi ghi tilesfile mirror is_shaded wi hi=
object (self)

  inherit game_graphic_object nm gwi ghi tilesfile mirror is_shaded wi hi
  initializer
    self#init_bcentre()

   end;;

(*
class game_object nm gwi ghi tilesfile mirror is_shaded wi hi=
object(self)
  inherit game_graphic_object nm gwi ghi tilesfile mirror is_shaded wi hi
  inherit game_object_NEW nm wi hi

  initializer
    self#init_bcentre()

end;;
*)

(** FROM POCENGINE *)
(** graphical canvas *)
class canvas =
object(self)
  val mutable objs_list=RefList.empty()

  val mutable tile_list=RefList.empty()

  val mutable del_list=RefList.empty()

  method clear()=
    objs_list<-RefList.empty();
    tile_list<-RefList.empty();
    del_list<-RefList.empty();

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
(*    o#set_graphic(); *)
    RefList.add objs_list o;

  method del_obj (od:game_object)=
  RefList.filter
    ( fun o->
	if o#get_name=od#get_name 
	  && o#get_rect#get_x=od#get_rect#get_x 
	  && o#get_rect#get_y=od#get_rect#get_y
	then false else true
    )
    objs_list


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
	    o#init_put(); 
	(
	  
(*	  o#put_shadow vx vy tw th;  *)
	  o#put vx vy tw th;	
	)
   )


end;;



class canvas_NEW =
object(self)
  val mutable objs_list=RefList.empty()

  val mutable tile_list=RefList.empty()

  val mutable del_list=RefList.empty()

  method clear()=
    objs_list<-RefList.empty();
    tile_list<-RefList.empty();
    del_list<-RefList.empty();

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


  method add_obj (o:graphic_generic_object)=    
(*    o#set_graphic(); *)
    RefList.add objs_list o;


  method del_obj (od:graphic_generic_object)=
  RefList.filter
    ( fun o->
	  if o#get_rect#get_x=od#get_rect#get_x 
	  && o#get_rect#get_y=od#get_rect#get_y
	then false else true
    )
    objs_list

(*
  method del_dead ()=
    RefList.filter 
      ( fun o->
	  if o#will_i_dead=false then true else false	     	    
      )
      objs_list
*) 

  method sort_obj (f:graphic_generic_object->graphic_generic_object->int)=
    RefList.sort ~cmp:f objs_list;

  method foreach_obj (f:graphic_generic_object->unit)=
      RefList.iter f objs_list


(** sort tiles to put from position *)

  method sort_position()=
    self#sort_obj 
      ( fun ao bo ->
	  match ao#get_rect with
	    | x when (x#get_y < bo#get_rect#get_y ) -> -1
	    | x when (x#get_y= bo#get_rect#get_y*32)  -> 0
	    | x when (x#get_y > bo#get_rect#get_y) -> 1
	    | x when (x#get_x < bo#get_rect#get_x) -> -1
	    | x when (x#get_x = bo#get_rect#get_x ) -> 0
	    | x when (x#get_x> bo#get_rect#get_x) -> 1

	    | _ -> 0
      );

  (** refresh the canvas. Refresh graphic part of each object *)
 method refresh (vx:int) (vy:int) (tw:int) (th:int)=

   self#sort_position();
   self#sort_layer();
   self#foreach_obj (
      fun o->
	let ox=o#get_rect#get_x and
	    oy=o#get_rect#get_y and
	    ow=o#get_rect#get_w and
	    oh=o#get_rect#get_h in
	  
	if 
	  (ox+ow)>vx & (oy+oh)>vy & vx<(ox+800) & vy<(oy+600) then 

	(
	  let nx=ox-vx and
	      ny=oy-vy in

	    o#move nx ny;
	    o#put();	
	    o#move ox oy
	)
   )


end;;

let none_obj=(new game_object "none" (!tile_w) (!tile_h) "none" false false 1 1 );;
let none_generic_obj=(new game_generic_object "none" (!tile_w) (!tile_h) 1 1 );;

class game_object_types=
object
  inherit [game_object] obj_types none_obj
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



class ['a] obj_layer_hash iv wi hi max=
object(self)
  inherit ['a] obj_layer iv wi hi max as super


  val mutable hash=Hashtbl.create 2
  val mutable hash_rev=Hashtbl.create 2

  
  method add_hash (k:string) (n:int)=Hashtbl.add hash k n;Hashtbl.add hash_rev n k
  method replace_hash k n=
    let i=self#get_hash k in
      self#del_hash k;
      self#del_hash_rev i;
      self#add_hash n i;
  method get_hash k=Hashtbl.find hash k
  method del_hash k=Hashtbl.remove hash k
 
  method get_hash_rev n=Hashtbl.find hash_rev n
  method del_hash_rev n=Hashtbl.remove hash_rev n

  method is_hash k=Hashtbl.mem hash k

  method del_hash_object (k:string)=
    let n=self#get_hash k in
    self#del_object n;
    self#del_hash k;
    self#del_hash_rev n

  method get_hash_object (k:string)=
    self#get_object (self#get_hash k)
end;;

class game_object_layer_hash wi hi max=
object(self)
  inherit [game_object] obj_layer_hash none_obj wi hi max as super
(*
  method init_put()=
    self#foreach_object (fun k o->
			o#init_put();
		     );
*)
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


