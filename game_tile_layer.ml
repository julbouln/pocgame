
open Value_common;;
open Value_xml;;
open Value_lua;;

open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_graphic;;
open Core_drawing;;

open Binding;;

open Game_layer;;
open Game_visual;;


(** Game tile layer class definition *)


class game_generic_tile_layer w h tw th=
object(self)
  inherit generic_object
  inherit [int] game_generic_layer w h as super    
  inherit lua_object
  inherit xml_object  

  method xml_to_init()=
    xml<-new xml_node;
    xml#set_tag "game_tile_layer";
    xml#add_attrib ("id",self#get_id);
    xml#set_pcdata 
      (String.concat "|" 
       (List.map (fun p->
		    match p with
		      | Some i->string_of_int i
		      | None -> ""
		 ) self#to_list))

  method xml_of_init()=
    let tl=Str.split_delim (Str.regexp "|") xml#pcdata in
      self#from_list (
	List.map (
	  fun t->
	    match t with 
	      | x when x<>"" -> Some (int_of_string t)
	      | _ -> None
	) tl);

      
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

  method lua_init()=
(*    ignore(tiles#lua_init()); 
    self#lua_parent_of "tiles" (tiles:>lua_object); 
*)
    lua#set_val (OLuaVal.String "set_tile_position") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **-> OLuaVal.value **->> OLuaVal.unit) 
	 (fun x y v->
	      match v with 
		| OLuaVal.Number i -> self#set_position x y (Some (int_of_float i))
		| OLuaVal.Nil -> self#set_position x y None
		| _ -> ()
	 )
      );

    lua#set_val (OLuaVal.String "get_tile_position") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.value) 
	 (fun x y->
	    let n=self#get_position x y in
	      match n with 
		| Some i -> OLuaVal.Number (float i)
		| None -> OLuaVal.Nil
	 )
      );

    super#lua_init()
	
end;;


