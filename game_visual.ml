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

open Value_lua;;

open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_main;;
open Core_event;;
open Core_fun;;

open Binding;;

(** Visual *)

class game_visual vx vy=
object(self)
  inherit lua_object as lo 

  val mutable fnode=new core_fun_node
  method get_fnode=fnode

  method fun_init()=
    fnode#set_id "visual";
    fnode#set_fun self#functionize;

  method functionize : functionizer=
    `GameVisualFun
      (self :> game_visual_fun)

  method get_id="visual"
 val mutable rect=new rectangle vx vy (video#get_w) (video#get_h)
  val mutable change=false

  method is_in x y=
     x>=rect#get_x && y>=rect#get_y && x<rect#get_x+rect#get_w && y<rect#get_y+rect#get_h

  method foreach_in_visual (f:int->int->unit)=
    for i=(rect#get_x/32)-1 to (rect#get_x/32) + (rect#get_w/32)+1 do
      for j=(rect#get_y/32)-1 to (rect#get_y/32) + (rect#get_h/32)+1 do
	f i j;
      done;
    done;


  method reinit()=change<-false
  method set_position x y=rect#set_position x y;change<-true
  method scroll x y=
    rect#set_position (rect#get_x+x) (rect#get_y+y);change<-true
  method has_change=change
  method get_x=rect#get_x
  method get_w=rect#get_w
  method get_y=rect#get_y		      
  method get_h=rect#get_h


  method lua_init()=

    lua#set_val (OLuaVal.String "scroll") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#scroll);
    lua#set_val (OLuaVal.String "set_position") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#set_position);

    lua#set_val (OLuaVal.String "get_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_x));
    lua#set_val (OLuaVal.String "get_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_y));
    lo#lua_init();

end;;

