(*open Unix;;

open Oxml;;
*)
open Object;;



open Rect;;
(* more generic parent - without graphic *)
class obj (nm:string) (wi:int) (hi:int) (gwi:int) (ghi:int)=
object
    val mutable name=nm
    method get_name=name
    method set_name n=name<-n

    val mutable rect=new rectangle 0 0 wi hi
    method get_rect=rect

(* go in obj ? *)
    val mutable prect=new rectangle 0 0 gwi ghi
    method get_prect=prect




    method update_prect()=
      let px=prect#get_x and
	py=prect#get_y and
	cx=rect#get_x and
	cy=rect#get_y in
      let xdif= 32-px and
	ydif= 32-py in

	if px<0 then (rect#set_position (cx-1) cy;prect#set_position (32+px) py);
	if px>32 then (rect#set_position (cx+1) cy;prect#set_position (px-32) py);
      	if py<0 then (rect#set_position cx (cy-1);prect#set_position px (32+py));
	if py>32 then (rect#set_position cx (cy+1);prect#set_position px (py-32));


    val mutable direction=0
    method get_direction=direction
    method turn dir=direction<-dir;



end;;


(** Game object type class definition *)

(* object_types are dynamic for create unit of type *)
(* objects are static for get caracteristic *) 

exception No_game_obj_type of string;;

class ['a] game_obj_types (none_obj:'a)=
  object
    val mutable object_types=let a=Hashtbl.create 2 in Hashtbl.add a "none" (function()->
(none_obj

));a
    val mutable objects=let a=Hashtbl.create 2 in Hashtbl.add a "none" (
none_obj
)
;a
    method add_object_type nm (obj:unit->'a)=
      if(Hashtbl.mem objects nm)==false then
	Hashtbl.add objects nm (obj());	  
      Hashtbl.add object_types nm obj
    method get_object_type nm=
      try
	(Hashtbl.find object_types nm)()
      with Not_found -> raise (No_game_obj_type nm)
    method get_object nm=      
      (Hashtbl.find objects nm)

    method is_object_type nm=(Hashtbl.mem objects nm)

    method count_objects_type=Hashtbl.length objects			       
    method foreach_object_type f= 
      Hashtbl.iter f object_types

end;;
