open Rect;;

(** Game layer class definition *)


exception Out_of_array of (int*int);;

(* NEW *)

class ['a] game_generic_layer wi hi=
object (self)
  val mutable rect=new rectangle 0 0 wi hi
  method get_rect=rect
    
  val mutable lay=
    Array.make_matrix wi hi None
      
  method resize nw nh=
    lay<-Array.make_matrix nw nh None;
    rect<-new rectangle 0 0 nw nh;

  method to_list=
    let a=DynArray.create() in
      Array.iter (
	fun slay->
	  Array.iter (
	    fun p->
	      DynArray.add a p
	  ) slay
      ) lay;
      DynArray.to_list a
    
  method out_of_lay x y=
    if x>=0 && y>=0 && x<rect#get_w && y<rect#get_h then false else true 
      
  (* FIXME : unsafe *)
  (* NOTE : only used by minimap *)
  method foreach_map_entry d=
    Array.iteri 
      (fun i v->
	 (
	   Array.iteri 
	     (fun j w->
		(d i j w)
	     ) v
	 )
      )  lay
      
  method foreach_map_entry1 (d:int->int->unit)=
    for i= -1 to Array.length lay +1 do
      for j= -1 to Array.length (lay.(0)) +1 do
	d j i 
      done;
    done;
    
  method clear()=
    self#foreach_map_entry (fun i j v->self#set_position i j None)
      
  method get_lay=lay 
  method set_lay l=lay<-l
    
  method get_position x y=
    try
      lay.(x).(y)
    with Invalid_argument v -> None(*raise (Out_of_array (x,y))*)
      
  method set_position x y (v:'a option)=
    try
      lay.(x).(y)<-v
    with Invalid_argument v -> () (*raise (Out_of_array (x,y)) *)
            
end;;


class game_layer wi hi=
object
  inherit [int] game_generic_layer wi hi
end;;
