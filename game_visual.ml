(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003 POC 

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

open Rect;;
open Video;;

open Low;;
open Medias;;

open Main;;

class game_visual vx vy=
object
  val mutable rect=new rectangle vx vy (video#get_w) (video#get_h)
  val mutable change=false

  val mutable black=new graphic_real_object "outofmap" (tile_box 32 32 (0,0,0));

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
end;;

(* FIXME : go in game_visual.ml *)
type scroll_dir=
| ScrollLeft
| ScrollRight
| ScrollUp
| ScrollDown
| NoScroll;;


class mouse_scroll s b=
object
  val mutable border=b
  val mutable scroll=s  

  val mutable dir=NoScroll

  method scroll (vrect:game_visual)=
    match dir with
      | ScrollLeft -> vrect#scroll (-scroll) 0
      | ScrollRight ->vrect#scroll (scroll) 0
      | ScrollUp ->vrect#scroll 0 (-scroll)
      | ScrollDown ->vrect#scroll 0 (scroll)
      | NoScroll -> ();
    

  method parse e=
    dir<-NoScroll;
    if e.ex<border then dir<-ScrollLeft;
    if e.ex>(main#scr_w-border) then dir<-ScrollRight;
    if e.ey<border then dir<-ScrollUp;
    if e.ey>(main#scr_h-border) then dir<-ScrollDown;
    
     
end;;
