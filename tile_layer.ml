open Low;;
open Rect;;
open Video;;
open Object;;
open Layer;;

open Game_visual;;

(** Game tile layer class definition *)


class game_generic_tile_layer w h tw th=
object(self)
  inherit layer w h as super
    
  val mutable border_layer=new layer w h

  method set_border_position  px py v=
    border_layer#set_position px py v

  method update()=()
(*    self#update_border 6 9;  *)


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

end;;

class game_tile_layer w h tw th file =
object(self)
  inherit game_generic_tile_layer w h tw th as super
val mutable tiles=new graphic_object_alpha tw th file false false false
  val mutable black=new graphic_real_object ("black_"^string_of_int tw^"x"^string_of_int th) (tile_box tw th (0,0,0));
  val mutable borders=new graphic_object tw th "medias/tiles/bordures.png" false false


  method put_map (vrect:game_visual)=
    super#foreach_map_entry1 (
      fun x y ->
	if self#out_of_lay x y then
	  self#put_black x y vrect
	else (
	  self#put x y vrect; 
(*	  self#put_border x y vrect  *)
	)
    );
    
  method put x y vrect=
    tiles#move ((x*tw)-vrect#get_x) ((y*th)-vrect#get_y);      
    tiles#set_cur_tile (super#get_position x y);
    tiles#put();

  method put_border x y (vrect:game_visual)=
      borders#move ((x*tw)-vrect#get_x) ((y*th)-vrect#get_y);      
      borders#set_cur_tile (super#get_position x y);
      borders#put();

  method put_black x y (vrect:game_visual)=
    black#move ((x*tw)-vrect#get_x) ((y*th)-vrect#get_y);      
    black#set_cur_tile (0);
    black#put();


	
end;;


exception Border_out_of_array of (int*int);;
exception Shaded_out_of_array of (int*int);;

class tile_layer wi hi tw th=
  object (self)
    inherit layer wi hi as super
	(* val mutable lay : int array array *) 

    val mutable terrains_tiles=
      new graphic_object_alpha (!tile_w) (!tile_h) "medias/tiles/terrains.png" false true false
    val mutable borders_tiles=
      new graphic_object (!tile_w) (!tile_h) "medias/tiles/bordures.png" false true 
    val mutable fow_tiles=
      new graphic_object (!tile_w) (!tile_h) "medias/tiles/fow.png" false false
	
    val mutable btile=
(*      tile_box (!scr_w) (!scr_h) (0,0,0) *)
      tile_empty(); 
      

    val mutable border_lay=Array.make_matrix wi hi 0
    method get_border x y=
      try
	border_lay.(x).(y)
      with Invalid_argument v -> raise (Border_out_of_array (x,y))

    val mutable nu_lay=Array.make_matrix wi hi false
    val mutable nu_shaded_lay=Array.make_matrix wi hi false
    method get_shaded x y=
      try
	nu_shaded_lay.(x).(y)
      with Invalid_argument v -> raise (Shaded_out_of_array (x,y))

    val mutable new_lay=Array.make_matrix wi hi 0

    val mutable fow_border_lay=Array.make_matrix wi hi (-1)

    method free_btile()=tile_free btile
    method get_btile=btile
	      

    method set_border_position x y v=
      if border_lay.(x).(y)<>v then
	nu_lay.(x).(y)<-true;

      border_lay.(x).(y)<-v;

    method set_fow_border_position x y v=      
      if fow_border_lay.(x).(y)<>v then
	nu_lay.(x).(y)<-true;
      fow_border_lay.(x).(y)<-v;


    method refresh x y=
      if new_lay.(x).(y)<>super#get_position x y then
	(
	 super#set_position x y (new_lay.(x).(y));
	 nu_lay.(x).(y)<-true;
	)

    method refresh_force x y=
      	 nu_lay.(x).(y)<-true;

    method set_shaded x y (v:bool)=
      if nu_shaded_lay.(x).(y)<>v then
	nu_lay.(x).(y)<-true;
      nu_shaded_lay.(x).(y)<-v;
      
    method set_position x y v=
      let mt=((super#get_position x y) mod 3) in
      new_lay.(x).(y)<-(v+mt)
    

    method reput (fow_lay:int array array) x y vx vy=
      if nu_lay.(x).(y)==true then (
	if nu_shaded_lay.(x).(y)==false then (
	  self#put_to x y vx vy false;      
	 ) else (
	  self#put_to x y vx vy true;      
	 );
	self#put_fow_to fow_lay x y vx vy; 
      )

    method reput_fow (fow_lay:int array array) x y vx vy=
      if nu_lay.(x).(y)==true then (
	self#put_fow_to fow_lay x y vx vy;
      );

    method put_fow_to (fow_lay:int array array) x y vx vy=      
      let b=fow_border_lay.(x).(y) 
      and n=fow_lay.(x).(y) in
	if b<>(-1) then (
	  fow_tiles#move ((x-vx)* !tile_w - 16) ((y-vy)* !tile_h - 16); 
(*	  fow_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h);  *)
	  fow_tiles#set_cur_tile b;
	  fow_tiles#put_to btile;  
	  nu_lay.(x).(y)<-true;
	);
	if n=0 then (
	  fow_tiles#move ((x-vx)* !tile_w - 16) ((y-vy)* !tile_h - 16); 
(*	  fow_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h); *)
	  fow_tiles#set_cur_tile 0;
	  fow_tiles#put_to btile;  
	  nu_lay.(x).(y)<-true;
	);

(* put direct to screen *)
    method reput_direct (fow_lay:int array array) x y vx vy=
	if self#get_shaded x y==false then (
	  self#put_direct x y vx vy false;      
	 ) else (
	  self#put_direct x y vx vy true;      
	 );
	self#put_fow_direct fow_lay x y vx vy; 


    method put_fow_direct (fow_lay:int array array) x y vx vy=      
      let b=fow_border_lay.(x).(y) 
      and n=fow_lay.(x).(y) in
	if b<>(-1) then (
	  fow_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h);  
	  fow_tiles#set_cur_tile b;
	  fow_tiles#put();  
	);
	if n=0 then (
	  fow_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h); 
	  fow_tiles#set_cur_tile 0;
	  fow_tiles#put();  
	);


    method put_direct x y vx vy shaded=
      let blay=self#get_border x y in
      terrains_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h);      
	terrains_tiles#set_cur_tile (super#get_position x y);
	if shaded==true then (
	  terrains_tiles#put_shaded();
	  if blay>0 then (
	    borders_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h);      
	    borders_tiles#set_cur_tile (blay - 1);
	    borders_tiles#put()
	  );
	)
	else (
	  
	  terrains_tiles#put(); 
	  if blay>0 then (
	    borders_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h);	    
	    borders_tiles#set_cur_tile (blay - 1);
	    borders_tiles#put();
	  );
	);

(* end direct put *)

    method put_to x y vx vy shaded =
      let blay=border_lay.(x).(y) in
(*      terrains_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h);      *)
      terrains_tiles#move ((x-vx)* !tile_w - 16) ((y-vy)* !tile_h - 16);     
      terrains_tiles#set_cur_tile (super#get_position x y);
      if shaded==true then (
	terrains_tiles#put_shaded_to btile;
	if blay>0 then (
(*	  borders_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h);      *)
	  borders_tiles#move ((x-vx)* !tile_w - 16) ((y-vy)* !tile_h - 16); 
	  borders_tiles#set_cur_tile (blay - 1);
	  borders_tiles#put_shaded_to btile
	 );
	nu_lay.(x).(y)<-false;
       )
      else (
	
	terrains_tiles#put_to btile; 
	if blay>0 then (
(*	  borders_tiles#move ((x-vx)* !tile_w) ((y-vy)* !tile_h);      *)
	  borders_tiles#move ((x-vx)* !tile_w - 16) ((y-vy)* !tile_h - 16);      
	  borders_tiles#set_cur_tile (blay - 1);
	  borders_tiles#put_to btile;
	 );
	nu_lay.(x).(y)<-false;
       );

    method set_border px py f t=
      let get_pos x y=self#get_position x y in
      let up()=self#set_position px py f in self#refresh_force px py;
      if get_pos px py=f || get_pos px py=(f+1) || get_pos px py=(f+2)   
then (
	let a=Array.make_matrix 3 3 (-1) in
	for v=(-1) to 1 do
	  for w=(-1) to 1 do
	    if (v+px)>0 && (w+py)>0 && (v+px)<rect#get_w && (w+py)<rect#get_h then (	       
		a.(w+1).(v+1)<-(		
		  let r=ref (get_pos (v+px) (w+py)) in
		  if (!r=6 || !r=7 || !r=8) && (f<>6 && f<>7 && f<>8) then r:=-1;
		  if (!r=0 || !r=1 || !r=2) && (t<>0 && t<>1 && t<>2) then r:=-2;
		  if !r=f || !r=(f+1) || !r=(f+2) then r:=6;		  
		  if !r=t || !r=(t+1) || !r=(t+2) then r:=0;
		  !r
		 )
	       )		     
	    done;
	  done;
	  match a with
	    | [|
		[|v;0;w|];
		[|6;6;6|];
		[|x;y;z|];
	      |] -> (self#set_border_position px py (3+(t/3*20));up())
	    | [|
		[|0;0;0|];
		[|0;6;z|];
		[|0;x;y|];
	      |] -> (self#set_border_position px py (11+(t/3*20));up())
	    | [|
		[|0;0;0|];
		[|x;6;0|];
		[|y;z;0|];
	      |] -> (self#set_border_position px py (12+(t/3*20));up())
	    | [|
		[|0;0;v|];
		[|0;6;z|];
		[|w;x;y|];
	      |] -> (self#set_border_position px py (5+(t/3*20));up())
	    | [|
		[|v;0;0|];
		[|x;6;0|];
		[|y;z;w|];
	      |] -> (self#set_border_position px py (7+(t/3*20));up())
	    | [|
		[|v;6;x|];
		[|0;6;y|];
		[|w;6;z|];
	      |] -> (self#set_border_position px py (1+(t/3*20));up())
	    | [|
		[|x;y;z|];
		[|6;6;6|];
		[|v;0;w|];
	      |] -> (self#set_border_position px py (4+(t/3*20));up())
	    | [|
		[|0;x;y|];
		[|0;6;z|];
		[|0;0;0|];
	      |] -> (self#set_border_position px py (10+(t/3*20));up())
	    | [|
		[|y;z;0|];
		[|x;6;0|];
		[|0;0;0|];
	      |] -> (self#set_border_position px py (9+(t/3*20));up())
	    | [|
		[|v;x;y|];
		[|0;6;z|];
		[|0;0;w|];
	      |] -> (self#set_border_position px py (8+(t/3*20));up())
	    | [|
		[|y;z;v|];
		[|x;6;0|];
		[|w;0;0|];
	      |] -> (self#set_border_position px py (6+(t/3*20));up())
	    | [|
		[|x;6;v|];
		[|y;6;0|];
		[|z;6;w|];
	      |] -> (self#set_border_position px py (2+(t/3*20));up())
	    | _-> (self#set_border_position px py (-1)) ;
      ) 


    method  set_fow_border (fow_layer:int array array) px py f=
      let get_pos x y=
	fow_layer.(x).(y) 
      in
	if get_pos (px) (py)=f then (
	  let a=Array.make_matrix 3 3 (-1) in
	    for v=(-1) to 1 do
	      for w=(-1) to 1 do
		if (v+px)>0 && (w+py)>0 && (v+px)<rect#get_w && (w+py)<rect#get_h then (	       
		  let n=if (get_pos (v+px) (w+py))=1 then 2 else (get_pos (v+px) (w+py)) in
		    a.(w+1).(v+1)<-n;
		)		     
	      done;
	    done;
	    match a with
	      | [|
		  [|v;0;w|];
		  [|2;2;2|];
		  [|x;y;z|];
		|] -> (self#set_fow_border_position px py 3)
	      | [|
		  [|v;2;w|];
		  [|0;2;y|];
		  [|x;2;z|];
		|] -> (self#set_fow_border_position px py 2)
	      | [|
		  [|v;y;w|];
		  [|2;2;2|];
		  [|x;0;z|];
		|] -> (self#set_fow_border_position px py 4)
	      | [|
		  [|v;2;w|];
		  [|y;2;0|];
		  [|x;2;z|];
		|] -> (self#set_fow_border_position px py 1)
		  
	      | [|
		  [|0;0;0|];
		  [|0;2;z|];
		  [|0;y;v|];
		|] -> (self#set_fow_border_position px py 5)

	      | [|
		  [|0;0;x|];
		  [|0;2;z|];
		  [|w;y;v|];
		|] -> (self#set_fow_border_position px py 5)
	      | [|
		  [|0;0;0|];
		  [|z;2;0|];
		  [|v;y;0|];
		|] -> (self#set_fow_border_position px py 6)
	      | [|
		  [|w;0;0|];
		  [|z;2;0|];
		  [|v;y;x|];
		|] -> (self#set_fow_border_position px py 6)
		  
	      | [|
		  [|x;z;0|];
		  [|y;2;0|];
		  [|0;0;0|];
		|] -> (self#set_fow_border_position px py 7)

	      | [|
		  [|x;z;v|];
		  [|y;2;0|];
		  [|w;0;0|];
		|] -> (self#set_fow_border_position px py 7)
		  
	      | [|
		  [|0;z;x|];
		  [|0;2;y|];
		  [|0;0;0|];
		|] -> (self#set_fow_border_position px py 8)

	      | [|
		  [|v;z;x|];
		  [|0;2;y|];
		  [|0;0;w|];
		|] -> (self#set_fow_border_position px py 8)
		  
	      | _-> () ;
	) 
	  

    method update_all()=
      for i=0 to rect#get_w-1 do
	for j=0 to rect#get_h-1 do
	  self#set_border i j 6 9;
	  self#set_border i j 12 9;
	done;
      done;


    method update t vx vy vw vh=
      for i=vx to vx+vw do
	for j=vy to vy+vh do
	  if self#out_of_lay i j==false then (
	    self#set_border i j 3 t;
	    self#set_border i j 6 3; 
	    self#set_border i j 6 12; 
	    self#set_border i j 12 3; 
	    self#set_border i j 6 t; 
	    self#set_border i j 12 t;	    
	 )
	done;
      done;

    method update_fow (fow_layer:int array array) vx vy vw vh=
      for i=vx to vx+vw do
	for j=vy to vy+vh do
	  if self#out_of_lay i j==false then (	    
	    fow_border_lay.(i).(j)<-(-1);
	 )
	done;
      done;
      for i=vx to vx+vw do
	for j=vy to vy+vh do
	  if self#out_of_lay i j==false then (	    
	    self#set_fow_border fow_layer i j 2; 
	    self#set_fow_border fow_layer i j 1; 
	 )
	done;
      done;


    method put_NEW fow_layer (x:int) (y:int) (w:int) (h:int)=
      for i=0 to w/32 do
	for j=0 to h/32 do
	  self#reput_direct fow_layer (i+x/32) (j+y/32) (x/32) (y/32)
	done;
      done;

    method put (x:int) (y:int) (w:int) (h:int)=
       tile_put_rect btile x y w h 
(*      tile_put btile x y; *)
      
  end;;
