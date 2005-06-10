
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


(*
empty : nothing
T : tile itself (-1 in layer)
0 to 23 : associated border
*)

(*
<tile_brush id="brush1">
  <args>
  <val_size name="size" w="3" h="3"/>
  </args>
  <data>
    T|T|T
   |T|T|T
   |T|T|T
  </data>
</tile_brush>
*)

type borders=
  | BRight
  | BBottom
  | BLeft
  | BTop
  | BRightToTop
  | BLeftToTop
  | BLeftToBottom
  | BRightToBottom
(* missing *)
  | BBottomToRight
  | BBottomToLeft
  | BTopToLeft
  | BTopToRight
  | BUnknow
;;

exception Bad_border of int

let int_of_border=function
  | BRight -> 0
  | BBottom -> 1
  | BLeft -> 2
  | BTop -> 3
  | BRightToBottom -> 4
  | BLeftToBottom -> 5
  | BLeftToTop -> 6
  | BRightToTop -> 7
(* missing *)
  | BBottomToRight -> 20
  | BBottomToLeft -> 21
  | BTopToLeft -> 22
  | BTopToRight -> 23
  | BUnknow -> -1
;;

let border_of_int=function
  | 0 -> BRight
  | 1 -> BBottom
  | 2 -> BLeft
  | 3 -> BTop
  | 4 -> BRightToBottom
  | 5 -> BLeftToBottom
  | 6 -> BLeftToTop
  | 7 -> BRightToTop
(* missing *)
  | 20 -> BBottomToRight
  | 21 -> BBottomToLeft
  | 22 -> BTopToLeft
  | 23 -> BTopToRight
  | x -> raise (Bad_border x);;


class tile_brush w h=
object(self)
  inherit generic_object
  inherit [int] game_generic_layer w h as super
  inherit lua_object

  val mutable get_pos_t=fun x y->None
  val mutable get_pos_b=fun x y->None

  val mutable set_pos_t=fun x y v->()
  val mutable set_pos_b=fun x y v->()

  method set_tile_layer (t:(int)game_generic_layer)=
    get_pos_t<-t#get_position;
    set_pos_t<-t#set_position;

  method set_border_layer (b:(int)game_generic_layer)=
    get_pos_b<-b#get_position;
    set_pos_b<-b#set_position;



  method paint x y t=
    self#foreach_map_entry(
      fun bx by bv->
	match bv with
	  | None ->()
	  | Some (-1) ->set_pos_t (x+bx) (y+by) (Some (t*3))	      
	  | Some o->set_pos_b (x+bx) (y+by) (Some (t*24+o))

    )

(*
  method xml_of_init()=
    let tl=Str.split_delim (Str.regexp "|") xml#pcdata in
      self#from_list (
	List.map (
	  fun t->
	    match t with 
	      | x when x="" -> None
	      | x when x="T" -> Some(-1)
	      | x -> Some (int_of_string t)
	) tl);
*)
end;;


(** Game tile layer class definition *)


class game_generic_tile_layer w h tw th=
object(self)
  inherit generic_object
  inherit [int] game_generic_layer w h as super    
  inherit lua_object
  inherit xml_object  


  method auto_border x y=
    let cpos=(self#get_position x y) in
    let cposmod=match cpos with
      | Some i ->i
      | None -> 0 in
    let b=fun x y->      
      let pos=(self#get_position x y) in
      
      match (pos) with
	| Some i->  
	    let pmod=(i mod 24) in
(*	      if pmod=cposmod mod 24 then *)
		border_of_int (pmod)
(*	      else BUnknow *)
	| None -> BUnknow in
    let r=
    match [b (x-1) (y-1);b x (y-1);b (x+1) (y-1);
	   b (x-1) (y);b x y;b (x+1) (y);
	   b (x-1) (y+1);b x (y+1);b (x+1) (y+1)] with
(*      | [_   ;_;_   ;
	 BTop;x;BTop;
	 _   ;_;_   ] ->BTop


      | [_      ;_;_   ;
	 BBottom;x;BBottom;
	 _      ;_;_   ] ->BBottom

      | [_;BLeft;_;
	 _;x    ;_;
	 _;BLeft;_] ->BLeft

      | [_;BRight;_;
	 _;x    ;_;
	 _;BRight;_] ->BRight
*)
(* *)
      | [_   ;_     ;_;
	 BTop;x     ;_;
	 _   ;BRight;_] ->BTopToRight

      | [_   ;_     ;_;
	 BRightToTop;x     ;_;
	 _   ;BRight;_] ->BTopToRight

      | [_   ;_     ;_;
	 BTop;x     ;_;
	 _   ;BRightToTop;_] ->BTopToRight

      | [_   ;_     ;_;
	 BRightToTop;x     ;_;
	 _   ;BRightToTop;_] ->BTopToRight
(* *)

      | [_      ;BRight;_;
	 BBottom;x     ;_;
	 _      ;_     ;_] ->BBottomToRight

      | [_      ;BRight;_;
	 BRightToBottom;x     ;_;
	 _      ;_     ;_] ->BBottomToRight

      | [_      ;BRightToBottom;_;
	 BBottom;x     ;_;
	 _      ;_     ;_] ->BBottomToRight

      | [_      ;BRightToBottom;_;
	 BRightToBottom;x     ;_;
	 _      ;_     ;_] ->BBottomToRight

(* *)

      | [_;_     ;_   ;
	 _;x     ;BTop;
	 _;BLeft;_   ] ->BTopToLeft

      | [_;_     ;_   ;
	 _;x     ;BTop;
	 _;BLeftToTop;_   ] ->BTopToLeft

      | [_;_     ;_   ;
	 _;x     ;BLeftToTop;
	 _;BTop;_   ] ->BTopToLeft

      | [_;_     ;_   ;
	 _;x     ;BLeftToTop;
	 _;BLeftToTop;_   ] ->BTopToLeft
(* *)

      | [_   ;BLeft;_   ;
	 _   ;x     ;BBottom;
	 _   ;_     ;_   ] ->BBottomToLeft

      | [_   ;BLeftToBottom;_   ;
	 _   ;x     ;BBottom;
	 _   ;_     ;_   ] ->BBottomToLeft

      | [_   ;BLeft;_   ;
	 _   ;x     ;BLeftToBottom;
	 _   ;_     ;_   ] ->BBottomToLeft

      | [_   ;BLeftToBottom;_   ;
	 _   ;x     ;BLeftToBottom;
	 _   ;_     ;_   ] ->BBottomToLeft

(* *)

      | [_      ;_     ;_;
	 BBottom;x     ;_;
	 _      ;BLeft;_] ->BLeftToBottom

      | [_      ;_     ;_;
	 BBottom;x     ;_;
	 _      ;BBottomToLeft;_] ->BLeftToBottom

      | [_      ;_     ;_;
	 BBottomToLeft;x     ;_;
	 _      ;BBottom;_] ->BLeftToBottom

      | [_      ;_     ;_;
	 BBottomToLeft;x     ;_;
	 _      ;BBottomToLeft;_] ->BLeftToBottom

	  

(* *)

      | [_      ;BLeft;_;
	 BTop;x    ;_;
	 _      ;_    ;_] ->BLeftToTop

      | [_      ;BLeft;_;
	 BTopToLeft;x    ;_;
	 _      ;_    ;_] ->BLeftToTop

      | [_      ;BTopToLeft;_;
	 BTop;x    ;_;
	 _      ;_    ;_] ->BLeftToTop

      | [_      ;BTopToLeft;_;
	 BTopToLeft;x    ;_;
	 _      ;_    ;_] ->BLeftToTop


(* *)

      | [_;_     ;_      ;
	 _;x     ;BBottom;
	 _;BRight;_      ] ->BRightToBottom


      | [_;_     ;_      ;
	 _;x     ;BBottom;
	 _;BBottomToRight;_      ] ->BRightToBottom

      | [_;_     ;_      ;
	 _;x     ;BBottomToRight;
	 _;BRight;_      ] ->BRightToBottom

      | [_;_     ;_      ;
	 _;x     ;BBottomToRight;
	 _;BBottomToRight;_      ] ->BRightToBottom


(* *)
      | [_   ;BRight;_   ;
	 _   ;x     ;BTop;
	 _   ;_     ;_   ] ->BRightToTop

      | [_   ;BRight;_   ;
	 _   ;x     ;BTopToRight;
	 _   ;_     ;_   ] ->BRightToTop

      | [_   ;BTopToRight;_   ;
	 _   ;x     ;BTop;
	 _   ;_     ;_   ] ->BRightToTop

      | [_   ;BTopToRight;_   ;
	 _   ;x     ;BTopToRight;
	 _   ;_     ;_   ] ->BRightToTop

(* *)

      | [_   ;_;_   ;
	 _;x;BTop;
	 _   ;_;_   ] ->BTop

      | [_   ;_;_   ;
	 BTop;x;_;
	 _   ;_;_   ] ->BTop

      | [_      ;_;_   ;
	 _;x;BBottom;
	 _      ;_;_   ] ->BBottom

      | [_      ;_;_   ;
	 BBottom;x;_;
	 _      ;_;_   ] ->BBottom

      | [_;BLeft;_;
	 _;x    ;_;
	 _;_;_] ->BLeft

      | [_;_;_;
	 _;x    ;_;
	 _;BLeft;_] ->BLeft

      | [_;_;_;
	 _;x    ;_;
	 _;BRight;_] ->BRight

      | [_;BRight;_;
	 _;x    ;_;
	 _;_;_] ->BRight

      | _ -> BUnknow in
      
      match r with
	| BUnknow -> ()
	| _-> self#set_position x y (Some ((int_of_border r)+24*(cposmod/24)))


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
    
    lua#set_val (OLuaVal.String "auto_tile_border") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) 
	 self#auto_border
      );

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


