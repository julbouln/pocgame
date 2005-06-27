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

(* FIXME : Go in game_interaction.ml *)

open Core_event;;
open Core_fun;;
open Core_interaction;;
open Core_main;;

type scroll_dir=
| ScrollLeft
| ScrollRight
| ScrollUp
| ScrollDown
| NoScroll;;


class interaction_mouse_scroll s b=
object(self)
  inherit interaction_lua

  val mutable border=b
  val mutable scroll=s  

  val mutable dir=NoScroll

  method on_loop()=
    let vis=fnode#get_parent#get_parent#get_children#get_object "visual" in
    let vrect=game_visual_of_fun vis#get_fun in
    match dir with
      | ScrollLeft -> vrect#scroll (-scroll) 0
      | ScrollRight ->vrect#scroll (scroll) 0
      | ScrollUp ->vrect#scroll 0 (-scroll)
      | ScrollDown ->vrect#scroll 0 (scroll)
      | NoScroll -> ();
    

  method ev_parser e=
    (match e with
       | EventMouse em ->
	   (match em with 
	      | MouseMotion(x,y) ->
		  self#parse x y
	      | _ ->()
	   )
       | _ -> ()
    )

  method private parse x y=
    dir<-NoScroll;
    if x<border then dir<-ScrollLeft;
    if x>(main#scr_w-border) then dir<-ScrollRight;
    if y<border then dir<-ScrollUp;
    if y>(main#scr_h-border) then dir<-ScrollDown;
    
     
end;;
