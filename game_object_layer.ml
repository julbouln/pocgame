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

open Value_common;;
open Value_object;;

open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_type;;

open Game_object;;
open Game_layer;;



(** Game object layer class definition *)


(* NEW *)

exception No_obj_at_position of (int*int);;

class ['a] game_obj_layer wi hi=
  object (self)
    inherit [string] game_generic_layer wi hi as super
    inherit ['a] generic_object_handler as oh


    method clear()=
      super#clear();
      oh#clear()


    method get_object_at_position x y=            
      let n=self#get_position x y in
	match n with
	  | Some k->(self#get_object k)
	  | None -> raise (No_obj_at_position (x,y))

    method is_object_at_position x y=
      match self#get_position x y with
	| Some v->true
	| None ->false
	  
    method update_obj_all()=
      super#clear();
      self#foreach_object(
	fun k ob->self#update_obj k;
      );
      
    method update_obj k=
      let obj=self#get_object k in
	obj#update_prect();
	if self#is_object k==true then (	  
	  obj#around_object self#out_of_lay 
	    (fun i j->
	       self#set_position i j (Some k);	    
	    )
	)
	  
    method move_object k x y=
      let obj=self#get_object k in
	self#set_position (x) (y) (Some k);
	self#set_position (obj#get_case_x) (obj#get_case_y) None;
	obj#move x y;


    method update_action()=
      self#foreach_object (fun k o->
		     	     o#act();
			)

  end;;



