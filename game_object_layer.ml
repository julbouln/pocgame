
open Generic;;
open Rect;;
open Video;;
open Medias;;

open Game_object;;
open Game_layer;;

open Otype;;

(** Game object layer class definition *)


(* NEW *)

exception No_obj_at_position of (int*int);;

class ['a] game_obj_layer wi hi=
  object (self)
    inherit [string] game_generic_layer wi hi as super
    inherit ['a] generic_object_handler


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



