
open Rect;;
open Video;;
open Medias;;
open Graphic;;
open Drawing;;
open Binding;;
open Game_layer;;
open Game_visual;;

(** Game tile layer class definition *)


class game_generic_tile_layer w h tw th=
object(self)
  inherit [int] game_generic_layer w h as super
    
  val mutable border_layer=new game_layer w h

  method get_border_layer_lay=border_layer#get_lay
  method set_border_layer_lay b=border_layer#set_lay b

  method set_border_position  px py v=
    border_layer#set_position px py v

  method get_border_position  px py=
    border_layer#get_position px py 
      
  method update()=()
    (*    self#update_border 6 9;  *)

(*
  method update_border t f=
  self#foreach_map_entry (fun x y k->
  self#set_border x y t f
  );

  method set_border px py t f=
    let get_pos x y=self#get_position x y in
      
    (* set temp 3x3 matrix *)
    let a=Array.make_matrix 3 3 (-1) in
      for v=(-1) to 1 do
	for w=(-1) to 1 do
	  if self#out_of_lay (v+px) (w+py)=false then
	    a.(w+1).(v+1)<-
	    let p=(get_pos (v+px) (w+py)) in
	      if p=f then -1 else
		if p=t then -2 else p
	done;
      done;
      
      (* match it *)
      match a with
	| [|
	    [| v;-1; w|];
	    [|-2;-2;-2|];
	    [| x; y; z|];
	  |] -> (self#set_border_position px py (3+(t/3*20)))
	| [|
	    [|-1;-1;-1|];
	    [|-1;-2;z|];
	    [|-1;x;y|];
	  |] -> (self#set_border_position px py (11+(t/3*20)))
	| [|
	    [|-1;-1;-1|];
	    [|x;-2;-1|];
	    [|y;z;-1|];
	  |] -> (self#set_border_position px py (12+(t/3*20)))
	| [|
	    [|-1;-1;v|];
	    [|-1;-2;z|];
	    [|w;x;y|];
	  |] -> (self#set_border_position px py (5+(t/3*20)))
	| [|
	    [|v;-1;-1|];
	    [|x;-2;-1|];
	    [|y;z;w|];
	  |] -> (self#set_border_position px py (7+(t/3*20)))
	| [|
	    [|v;-2;x|];
	    [|-1;-2;y|];
	    [|w;-2;z|];
	  |] -> (self#set_border_position px py (1+(t/3*20)))
	| [|
	    [|x;y;z|];
	    [|-2;-2;-2|];
	    [|v;-1;w|];
	  |] -> (self#set_border_position px py (4+(t/3*20)))
	| [|
	    [|-1;x;y|];
	    [|-1;-2;z|];
	    [|-1;-1;-1|];
	  |] -> (self#set_border_position px py (10+(t/3*20)))
	| [|
	    [|y;z;-1|];
	    [|x;-2;-1|];
	    [|-1;-1;-1|];
	  |] -> (self#set_border_position px py (9+(t/3*20)))
	| [|
	    [|v;x;y|];
	    [|-1;-2;z|];
	    [|-1;-1;w|];
	  |] -> (self#set_border_position px py (8+(t/3*20)))
	| [|
	    [|y;z;v|];
	    [|x;-2;-1|];
	    [|w;-1;-1|];
	  |] -> (self#set_border_position px py (6+(t/3*20)))
	| [|
	    [|x;-2;v|];
	    [|y;-2;-1|];
	    [|z;-2;w|];
	  |] -> (self#set_border_position px py (2+(t/3*20)))
	| _-> (self#set_border_position px py (-1)) ;
*)	    
end;;

class game_tile_layer w h tw th file =
object(self)
  inherit game_generic_tile_layer w h tw th as super
  val mutable tiles=new graphic_from_file "tiles" file tw th 
  method get_tiles=tiles
  val mutable black=new graphic_from_drawing "black" ("black_"^string_of_int tw^"x"^string_of_int th) 
    (fun()->
       let dr=drawing_vault#new_drawing() in 
	 dr#create tw th (0,0,0);
	 [|dr|])
(*val mutable black=new graphic_real_object ("black_"^string_of_int tw^"x"^string_of_int th)
 (tile_box tw th (0,0,0));
*)
(* FIXME : must be passed as arg *)
  val mutable borders=new graphic_from_file "borders" "medias/tiles/bordures.png" tw th 
  method get_borders=borders

  method put_map (vrect:game_visual)=
    vrect#foreach_in_visual (
      fun x y ->
	if self#out_of_lay x y then
	  self#put_black x y vrect
	else (
	  self#put x y vrect; 
	  if self#get_border_position x y<>None then
	    self#put_border x y vrect  
	)
    )
(*    super#foreach_map_entry1 (
      fun x y ->
	if self#out_of_lay x y then
	  self#put_black x y vrect
	else (
	  self#put x y vrect; 
(*	  self#put_border x y vrect  *)
	)
    );
*)    
  method put x y vrect=
    tiles#move ((x*tw)-vrect#get_x) ((y*th)-vrect#get_y);      
    (match (super#get_position x y) with
      | Some v->tiles#set_cur_drawing v;tiles#put();
      | None -> ());


  method put_border x y (vrect:game_visual)=
    borders#move ((x*tw)-vrect#get_x) ((y*th)-vrect#get_y);      
    (match (super#get_border_position x y) with
       | Some v->borders#set_cur_drawing v;borders#put();
       | None -> ());


  method put_black x y (vrect:game_visual)=
    black#move ((x*tw)-vrect#get_x) ((y*th)-vrect#get_y);      
    black#set_cur_drawing (0);
    black#put();


	
end;;


