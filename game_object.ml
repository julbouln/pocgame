(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Value_xml;;
open Value_val;;
open Value_lua;;

open Core_val;;
open Core_rect;;
open Core_medias;;
open Core_drawing;;
open Core_graphic;;
open Core_anim;;
open Core_action;;
open Core_type;;
open Core_timer;;
open Core_sprite;;

open Core_fun;;

open Binding;;



(** Main game object class *)
class game_object=
object(self)
  inherit sprite_object as super

    (** case position & size *)
    val mutable rect=new rectangle 0 0 0 0
    method get_rect=rect

    (** update prect from rect *)
    method update_prect()=
      let px=prect#get_x and
	  py=prect#get_y in
	rect#set_position (px/32) (py/32)

    (** direction *)
    val mutable direction=0
    method get_direction=direction

    (** scroll p, scroll p pixel in current direction *)
    method scroll p=
      let px=prect#get_x and
	  py=prect#get_y in 
	match direction with
	  | 0 -> prect#set_position px (py-p);
	  | 2 -> prect#set_position (px + p) py;
	  | 4 -> prect#set_position px (py+p); 
	  | 6 -> prect#set_position (px-p) py;
	  | _ -> ();

    (** return the next case from current position *)	      
    method next_position()=
      let x=rect#get_x and
	  y=rect#get_y in
      match direction with
	  | 0 -> (x,y-1);
	  | 2 -> (x+1,y);
	  | 4 -> (x,y+1); 
	  | 6 -> (x-1,y);
	  | _ -> (x,y);

 
    (** time *)
    val mutable time=new lua_timer
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
      super#act();
      time#step();

(* case blocking *)
  val mutable blocking=false;
  method set_blocking b=blocking<-b
  method get_blocking=blocking

  method resize w h=rect#set_size w h

  method set_case_position x y=
    rect#set_position x y
      
  method get_case_x=rect#get_x
  method get_case_y=rect#get_y
      
  method get_case_w=rect#get_w
  method get_case_h=rect#get_h
      
(** iter each object case *)
  method around_object out_of_map (f:int->int->unit)=
    for x=(self#get_case_x - self#get_case_w/2 ) to (self#get_case_x + self#get_case_w/2 ) do
      for y=(self#get_case_y - self#get_case_h/2 ) to (self#get_case_y + self#get_case_h/2 ) do
	    if out_of_map x y=false then f x y	    
	  done;
	done;

(** iter each object case +1*)
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

  (** move to case *)
  method move x y=
    self#set_case_position x y;
    prect#set_position (x*32) (y*32);
    self#graphics_update()

  (** turn to direction *)
  method turn d=
    direction<-d;
    self#graphics_update()
      
  (** baricentre *)    
  val mutable bcentre=(0,0)
  method get_bcentre_x=(fst bcentre)
  method get_bcentre_y=(snd bcentre)

  method init_bcentre gr_id=
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
	);
(*      print_string("GAME_OBJECT: bcentre : "^string_of_int (fst bcentre)^","^string_of_int (snd bcentre));print_newline();
*)
  method get_pixel_x=
    prect#get_x - (fst bcentre) + 16
    (*(rect#get_x*32) + prect#get_x - (fst bcentre) + 16 *)
  method get_pixel_y=
    prect#get_y - (snd bcentre) + 16
    (*(rect#get_y*32) + prect#get_y - (snd bcentre) + 16 *)

(** for fun *)
  method pixel_x()=self#get_pixel_x
  method pixel_y()=self#get_pixel_y
  method case_x()=self#get_case_x
  method case_y()=self#get_case_y
  method case_w()=self#get_case_w
  method case_h()=self#get_case_h
  method direction()=self#get_direction


  method functionize : functionizer=
    `GameObjectFun 
      (self :> game_object_fun)


  method lua_init()=
      lua#set_val (OLuaVal.String "move") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#move);
      lua#set_val (OLuaVal.String "get_case_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_case_x));
      lua#set_val (OLuaVal.String "get_case_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_case_y));

      lua#set_val (OLuaVal.String "get_bcentre_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_bcentre_x));
      lua#set_val (OLuaVal.String "get_bcentre_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_bcentre_y));


      lua#set_val (OLuaVal.String "scroll") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#scroll);
      lua#set_val (OLuaVal.String "next_position") 
	(OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.value) 
	   (fun()->
	     let np=self#next_position() in
	       lua_of_val_ext (`Position np)
	   )
	);

    lua#set_val (OLuaVal.String "get_pixel_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_pixel_x));
    lua#set_val (OLuaVal.String "get_pixel_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_pixel_y));
    lua#set_val (OLuaVal.String "init_bcentre") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (self#init_bcentre));


    ignore(time#lua_init()); 
    self#lua_parent_of "timer" (time:>lua_object);

    lua#set_val (OLuaVal.String "turn") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#turn);
    lua#set_val (OLuaVal.String "get_direction") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_direction));


    super#lua_init();

end;; 






