
open Ocommon;;

open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_graphic;;
open Core_drawing;;

open Binding;;

open Game_layer;;
open Game_visual;;

open Oxml;;
open Olua;;
(** Game tile layer class definition *)


class game_generic_tile_layer w h tw th=
object(self)
  inherit generic_object
  inherit [int] game_generic_layer w h as super    
  inherit lua_object

  method to_xml_string=
    let x=self#to_xml in
      Xml.to_string x

  method to_xml=
    Xml.Element
      ("game_tile_layer",[("id",self#get_id)],
       [Xml.PCData
	  (String.concat "|" 
	     (List.map (fun p->
			  match p with
			    | Some i->string_of_int i
			    | None -> ""
		       ) self#to_list))
       ]
      )

  method from_xml x=
    match x with
      | Xml.Element (id,attr,childs)->
	  List.iter (
	    fun c ->
	    match c with
	      | Xml.PCData d->
		  let tl=Str.split_delim (Str.regexp "|") d in
		    self#from_list (
		      List.map (
			fun t->
			  match t with 
			    | x when x<>"" -> Some (int_of_string t)
			    | _ -> None
		      ) tl);
	      | _ ->()
	  ) childs;
      | _ ->()


  method put_map (vrect:game_visual)=()
end;;

class game_tile_layer w h tw th file =
object(self)
  inherit game_generic_tile_layer w h tw th as super
  val mutable tiles=new graphic_from_file file tw th 
  method get_tiles=tiles
  val mutable black=new graphic_from_drawing ("black_"^string_of_int tw^"x"^string_of_int th) 
    (fun()->
       let dr=drawing_vault#new_drawing() in 
	 dr#create tw th (0,0,0);
	 [|dr|])

  method put_map (vrect:game_visual)=
    vrect#foreach_in_visual (
      fun x y ->
	if self#out_of_lay x y then
	  self#put_black x y vrect
	else (
	  self#put x y vrect; 
	)
    )

  method put x y vrect=
    tiles#move ((x*tw)-vrect#get_x) ((y*th)-vrect#get_y);      
    (match (super#get_position x y) with
      | Some v->tiles#set_cur_drawing v;tiles#put();
      | None -> ());

  method put_black x y (vrect:game_visual)=
    black#move ((x*tw)-vrect#get_x) ((y*th)-vrect#get_y);      
    black#set_cur_drawing (0);
    black#put();


	
end;;


