open Rect;;

(** Game layer class definition *)


exception Out_of_array of (int*int);;

class game_layer wi hi=
  object (self)
    val mutable rect=new rectangle 0 0 wi hi

    val mutable lay=
      Array.make_matrix wi hi (-1)

    method resize nw nh=
      lay<-Array.make_matrix nw nh (-1);
      rect<-new rectangle 0 0 nw nh;

    method init v=
      self#foreach_map_entry (
	fun x y cv->
	  self#set_position x y v
      );

    method out_of_lay x y=
      if x>=0 && y>=0 && x<rect#get_w && y<rect#get_h then false else true 

    method print_para w x y=print_string ("GAME LAYER PARACHUTE : "^w^" "^string_of_int(x)^"-"^string_of_int(y)^" OUT OF ARRAY");print_newline();

    method get_rect=rect

    (* FIXME : unsafe *)
    (* NOTE : only used by minimap *)
    method foreach_map_entry d=
      Array.iteri (fun i v->(Array.iteri (fun j w->(d i j w)) v))  lay

    method foreach_map_entry1 (d:int->int->unit)=
      for i= -1 to Array.length lay +1 do
	for j= -1 to Array.length (lay.(0)) +1 do
	  d j i 
	done;
      done;

    method clean()=
      self#foreach_map_entry (fun i j v->self#set_position i j 0)
	
    method get_lay=lay 
    method set_lay l=lay<-l

    method get_position x y=
      try
	lay.(x).(y)
      with Invalid_argument v -> 0(*raise (Out_of_array (x,y))*)
      
    method set_position x y v=
      try
	lay.(x).(y)<-v
      with Invalid_argument v -> () (*raise (Out_of_array (x,y)) *)


   end;;
