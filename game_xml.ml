open Oxml;;

open Game_object;;

type gm_object=
{
 oname:string;
 ofile:string;
 otype:string;
(* decal value *)
 odw:int;
 odh:int;
(* pixel *)
 ow:int;
 oh:int;
 oframes:int list;
 orefresh:int;
 ocw:int;
 och:int;
};;

class xml_gm_object_parser=
object
  inherit xml_parser
  val mutable name=""
  val mutable t=""
  val mutable file=""
  val mutable w=0
  val mutable h=0
  val mutable dw=0
  val mutable dh=0
  val mutable cw=0
  val mutable ch=0
  val mutable frames=[]
  val mutable refresh=0

  method get_val={oname=name;otype=t;ofile=file;odw=dw;odh=dh;ow=w;oh=h;oframes=frames;orefresh=refresh;ocw=cw;och=ch}

  method tag=""
  method parse_attr k v=
    match k with 
      | "name" -> name<-v
      | "type" -> t<-v
      | _ -> ()
   
  method parse_child k v=
    match k with
      | "file" -> let p=(new xml_string_parser "path") in p#parse v;file<-p#get_val    
      | "pixel_size" -> let p=(new xml_size_parser ) in p#parse v;w<-p#get_w;h<-p#get_h;
      | "case_size" -> let p=(new xml_size_parser ) in p#parse v;cw<-p#get_w;ch<-p#get_h;
      | "decal_value" -> let p=(new xml_size_parser ) in p#parse v;dw<-p#get_w;dh<-p#get_h;
      | "frames" -> let p=(new xml_intlist_parser "frame" (fun()->new xml_int_parser "frame"))  in p#parse v;frames<-p#get_list
      | "refresh" -> let p=(new xml_int_parser "value") in p#parse v;refresh<-p#get_val
      | _ -> ()

end;;

class xml_gm_objects_parser name=
object
  inherit [gm_object] xml_list_parser name (fun()->new xml_gm_object_parser)
end;;



type state={frames:int array;refresh:int;action:unit->unit;sound:string array};;

type state_anim=
    {mutable state_name:string;
     mutable anim_frames:int array;
     mutable anim_refresh:int;
     mutable sounds:string array
   };;

let state_anim2state s=
  {frames=s.anim_frames;
   refresh=s.anim_refresh;
   action=(function()->());
   sound=s.sounds
};;


class xml_state_parser=
object
  inherit xml_parser
val mutable name="none"
val mutable frames=[|0|] 
val mutable refresh=0;
val mutable snds=[|"none"|]
  method tag=""
method get_val=
    {state_name=name;
     anim_frames=frames;
     anim_refresh=refresh;
     sounds=snds
    }

  method parse_attr k v=
    match k with
      | "name"-> name<-v
      | _ -> ()
  method parse_child k v=
    match k with

      | "frames" -> let p=(new xml_intlist_parser "frame" (fun()->new xml_int_parser "frame"))  in p#parse v;frames<-p#get_array
      | "sounds" -> let p=(new xml_stringlist_parser "sound" (fun()->new xml_string_parser "sound"))  in p#parse v;snds<-p#get_array
      | "refresh" -> let p=(new xml_int_parser "value") in p#parse v;refresh<-p#get_val
      | _ -> ()


end;;

class xml_state_list_parser=
object
  inherit [state_anim] xml_list_parser "state" (fun()->new xml_state_parser)
end;;


exception Container_state_not_found of string;;

(* FIXME must be game_state_container *)

class game_state_container (states:state_anim array)=
object
  val mutable sts=Hashtbl.create 2;
initializer


  Array.iter (function s->
(*		print_string ("add state "^s.state_name);print_newline(); *)
		Hashtbl.add sts s.state_name (state_anim2state s);
	     ) states;

method is_state n=
  Hashtbl.mem sts n 
method get_state n=
  if Hashtbl.mem sts n then
    Hashtbl.find sts n
  else raise (Container_state_not_found n)

end;;

let none_stc=(new game_state_container [|{state_name="idle";anim_frames=[|0|];anim_refresh=0;sounds=[|"none"|]}|]);;


class xml_gm_object_with_state_parser=
object
  inherit xml_gm_object_parser as super

val mutable states=[||] 
  method parse_child k v=
    super#parse_child k v;
    match k with
    | "states" -> 
	let p=new xml_state_list_parser in
	  p#parse v;
	  states<-p#get_array
    | _ -> ()

  method get_states=states
  
(*
  method get_obj (f:string->string->string->int->int->int->int->game_state_container->'a)=f name t file w h cw ch (new game_state_container states)
*)

end;;


class ['a] xml_game_obj_parser (iv:'a)=
object
  inherit xml_gm_object_with_state_parser as super

  method get_obj (f:string->string->string->int->int->int->int->game_state_container->((unit->'a)*string))=f name t file w h cw ch (new game_state_container states)
end;;


class ['a] xml_game_objs_parser (name:string) (iv:'a) (f:string->string->string->int->int->int->int->game_state_container->((unit->'a)*string))=
object
  inherit [gm_object] xml_list_parser name (fun()->new xml_game_obj_parser iv) as super
  val mutable objs=DynArray.create()

  method parse_child k v=
    super#parse_child k v;
    match k with
      | ct -> let p=new xml_game_obj_parser iv in 
	  p#parse v;
	  DynArray.add objs (p#get_obj f)

  method get_objs=
    (DynArray.to_array objs : ((unit->'a)*string) array)
end;;
