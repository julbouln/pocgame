(*open Unix;;

open Oxml;;
*)
open Object;;



open Rect;;
(* more generic parent - without graphic *)
class obj (nm:string) (wi:int) (hi:int)=
object
    val mutable name=nm
    method get_name=name
    method set_name n=name<-n

    val mutable rect=new rectangle 0 0 wi hi
    method get_rect=rect
end;;


(** Game object type class definition *)

(* object_types are dynamic for create unit of type *)
(* objects are static for get caracteristic *) 


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
    method get_object_type nm=(Hashtbl.find object_types nm)()
    method get_object nm=(Hashtbl.find objects nm)
    method is_object_type nm=(Hashtbl.mem objects nm)
end;;
