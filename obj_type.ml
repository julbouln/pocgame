(*open Unix;;

open Oxml;;
*)
open Object;;

open Otype;;

open Rect;;
(* more generic parent - without graphic *)
class obj (nm:string) (wi:int) (hi:int) (gwi:int) (ghi:int)=
object
    val mutable name=nm
    method get_name=name
    method set_name n=name<-n

    val mutable id=""
    method get_id=id
    method set_id i=id<-i

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


class ['a] game_obj_types (none_obj:'a)=
object
  inherit ['a] obj_types none_obj
end;;
