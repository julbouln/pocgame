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


open Core_val;;
open Core_fun;;
open Core_action;;


class action_game=
object
  inherit action_lua
  method private get_object=
    let obj=fnode#get_parent#get_parent#get_parent in
      game_object_of_fun obj#get_fun

  method private get_object_map=
    let obj=fnode#get_parent#get_parent#get_parent#get_parent in
      game_object_map_of_fun obj#get_fun

  method private get_map=
    let obj=fnode#get_parent#get_parent#get_parent#get_parent#get_parent in
      game_map_of_fun obj#get_fun
end;;


class action_pathfinding=
object(self)
  inherit action_game as super

  val mutable vel=2

  val mutable current=(0,0)
  val mutable dest=(0,0)

  val mutable path=Array.create 1 (0,0)
  val mutable cur_path=0

  method on_start ve=
    cur_path<-0;
    current<-(self#get_object#case_x(),self#get_object#case_y());
    dest<-position_of_val (ve#get_val (`Int 0));
    
    path<-self#get_map#path_calc current dest;
    
    super#on_start ve

  val mutable cpx=0
  val mutable cpy=0		    

  method on_loop()=
    let (nx,ny)=
      if cur_path+1<Array.length path-1 then
	path.(cur_path+1)
      else
	dest
    in
    current<-(self#get_object#case_x(),self#get_object#case_y());
    let (dx,dy)=dest in
    let (cx,cy)=current in
      if dx<>cx || dy<>cy then
	(

	    cpx<-self#get_sprite#get_x();
	    cpy<-self#get_sprite#get_y();
	    
	    cpx<-cpx+ ((nx-cx)*vel);
	    cpy<-cpy+ ((ny-cy)*vel);

	    self#get_sprite#jump cpx cpy;

	    (match (nx-cx,ny-cy) with
	      | (-1,-1)->self#get_object#turn 7
	      | (-1,0)->self#get_object#turn 6
	      | (-1,1)->self#get_object#turn 5
	      | (0,-1)->self#get_object#turn 0
	      | (0,1)->self#get_object#turn 4
	      | (1,-1)->self#get_object#turn 1
	      | (1,0)->self#get_object#turn 2
	      | (1,1)->self#get_object#turn 3
	      | _ -> ());


	    if nx=cx && ny=cy then 
	      cur_path<-cur_path+1;
	    

	)
      else
	(
	  cur_path<-0;
	);




      super#on_loop()

end;;
