open Rect;;
(* DEPRECATED *)

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



class game_graphic_object nm gwi ghi tilesfile mirror  is_shaded wi hi=
object(self)
  inherit game_generic_object nm wi hi gwi ghi as super

    val mutable graphic=new graphic_object gwi ghi tilesfile mirror is_shaded 

    method act vx vy=
      super#act vx vy;
      let cur=self#graphic#get_cur_tile in
	self#graphic#set_cur_tile (((self#graphic#get_tiles_size)/8)*direction + 
				     self#get_current_state#get_frame);

    method move x y=
      super#move x y;
      self#graphics_update()


    method graphics_register (reg:canvas_object->unit)=
      reg (graphic:>canvas_object)
    method graphics_unregister (unreg:canvas_object->unit)=
      unreg (graphic:>canvas_object)
    method graphics_update ()=
      self#graphic#move (self#get_pixel_x) (self#get_pixel_y);

    method graphic=graphic
    method get_graphic=graphic
    method set_graphic()=graphic<-new graphic_object gwi ghi tilesfile mirror is_shaded



    (* baricentre *)

    val mutable bcentre=(0,0)
    method get_bcentre_x=(fst bcentre)
    method get_bcentre_y=(snd bcentre)

(*    method around_object out_of_map (f:int->int->unit)=
      let rpos=self#graphic#get_rpos in
      let x1=rpos#get_x/32 and
	  y1=rpos#get_y/32 and
	  x2=rpos#get_w/32 and
	  y2=rpos#get_h/32 in

      for x=(self#get_case_x + x1 -self#get_bcentre_x/32 -1) to (self#get_case_x + x2 -self#get_bcentre_x/32 -1) do
	for y=(self#get_case_y + y1 -self#get_bcentre_y/32 -1) to (self#get_case_y + y2 -self#get_bcentre_y/32 -1) do
	  if out_of_map x y=false then f x y	    
	done;
      done;
*)
    method init_bcentre()=
      let rpos=self#graphic#get_rpos in
      let x1=rpos#get_x and
	y1=rpos#get_y and
	x2=rpos#get_w and
	y2=rpos#get_h in

	bcentre<-
	(
	  (x1+x2)/2,
	  (y1+y2)/2
	)

    method init_bcentre_with (graph:graphic_object)=
      let rpos=graph#get_rpos in
      let x1=rpos#get_x and
	  y1=rpos#get_y and
	  x2=rpos#get_w and
	  y2=rpos#get_h in

	bcentre<-
	(
	  (x1+x2)/2,
	  (y1+y2)/2
	)

    method get_pixel_x=(rect#get_x*32) + prect#get_x - (fst bcentre) + 16
    method get_pixel_y=(rect#get_y*32) + prect#get_y - (snd bcentre) + 16

end;; 


class game_object nm gwi ghi tilesfile mirror is_shaded wi hi=
object (self)


  inherit game_graphic_object nm gwi ghi tilesfile mirror is_shaded wi hi
  initializer
    self#init_bcentre()

  method lua_register (interp:lua_interp)=
    interp#parse (id^"={}");
    props#lua_register id interp

end;;

class del_stack=
object
  val mutable del_stack=Stack.create();
  method add_del_stack k=Stack.push k del_stack; 
  method empty_del_stack (del_func:int->unit)=
    while Stack.length del_stack <> 0 do
      let a=(Stack.top del_stack) in
      del_func a;
	let s=Stack.pop del_stack in ();
    done;
end;;


class ['a] game_obj_layer (none_obj:'a) wi hi max=
  object (self)
    inherit game_layer wi hi as super
    val mutable stack=new del_stack
    val mutable objs=Array.make max none_obj
    val mutable is_objs=Array.make max false
    val mutable cur_obj=max-1


    method get_cur_obj=cur_obj  
(*    method get_objects=objs  *)
	
    method foreach_object d=
      let f i v=if is_objs.(i)==true then (if objs.(i)#get_name<>"none" then d i v) in 
      Array.iteri d objs;


    method foreach_map_object d=
      self#foreach_map_entry (fun i j v->(if is_objs.(v)==true then d i j objs.(v)))

    method clear()=
      for i=0 to wi-1 do
	for j=0 to hi-1 do
	  super#set_position i j 0
	done;
      done;

      for k=1 to cur_obj do
	objs.(k)<-none_obj
      done;
      
    method print_para_o w num=
      print_string ("GAME OBJECT LAYER PARACHUTE : "^w^" "^string_of_int(num)^" OUT OF ARRAY");
      print_newline();

    method out_of_a num=
      if num>=0 && num<=cur_obj then false else true

    method get_object_by_position x y=            
      let n=self#get_position x y in
      self#get_object n

    method get_object num=      
      objs.(num)

    method is_object x y=
      if self#get_position x y<>0 then true else false

    method is_object_num num=
      is_objs.(num);

    method is_object_num_with_check num=
      if self#out_of_a num==false then
	self#is_object_num num
      else false

    method set_object num obj=    
      objs.(num)<-obj

    method add_object obj=
      if  obj#get_name<>"none" then (
	let k=ref 1 in

	while self#is_object_num_with_check (!k)==true do k:=!k+1 done;
        if self#out_of_a (!k)==false then (
	  objs.(!k)<-obj;
	  is_objs.(!k)<-true;
	  self#set_position (obj#get_case_x) (obj#get_case_y) !k;
         )
       )

    method add_object_with_num obj=
	let k=ref 1 in
      if  obj#get_name<>"none" then (


	while self#is_object_num_with_check (!k)==true do k:=!k+1 done;
        if self#out_of_a (!k)==false then (
	  objs.(!k)<-obj;
	  is_objs.(!k)<-true;
	  self#set_position (obj#get_case_x) (obj#get_case_y) !k;
         )
       );
      !k

    method add_del_stack k=stack#add_del_stack k
    method empty_del_stack()=stack#empty_del_stack self#del_object;

    method del_object num=
      self#set_object num none_obj;
      is_objs.(num)<-false;

    method update_obj_all()=
      self#foreach_object(
      fun k ob->self#update_obj k;
    );
  
    method update_obj num=
      let obj=self#get_object num in
      if self#is_object_num num==true then (

	obj#around_object self#out_of_lay (fun i j->
					     self#set_position i j num;	    
					  )


      )
    method move num x y=
      let obj=objs.(num) in
      self#set_position (x) (y) num;
      self#set_position (obj#get_case_x) (obj#get_case_y) 0;
      obj#move x y;

    method move_from cx cy x y=
      let num=self#get_position cx cy in
      self#move num x y;


(*    method reput btile x y vx vy=
      (self#get_object_by_position x y)#put_to btile (vx*32) (vy*32) 32 32;
*)


    method reduce_objs()=
      let c=ref 1 in
	self#foreach_object (fun i v->(
			       if v#get_name<>"none" then
				 c:=!c+1
			     ));
	let a=Array.make (!c+1) (0,none_obj) in
	  c:=1;
	  self#foreach_object (fun i v->(
				 if v#get_name<>"none" then (
				   a.(!c)<-(i,v);
				   c:=!c+1
				 )));
	  a

  end;;


(** game_object layer *)
class game_object_layer wi hi max=
object(self)
  inherit [game_object] game_obj_layer none_obj wi hi max as super

  method update_obj num=
    let obj=self#get_object num in
      obj#update_prect();
      super#update_obj num;

  method update_action()=
    self#foreach_object (fun k o->
			   o#act 0 0;
			   o#anim();
			)
end;;



(** obj layer hash *)

exception Game_obj_hash_not_found of string;;

class ['a] game_obj_layer_hash iv wi hi max=
object(self)
  inherit ['a] game_obj_layer iv wi hi max as super

  method update_obj num=
    let obj=self#get_object num in
      obj#update_prect();
      super#update_obj num;

  method update_action()=
    self#foreach_object (fun k o->
			   o#act 0 0;
			   o#anim();
			)


  val mutable hash=Hashtbl.create 2
  val mutable hash_rev=Hashtbl.create 2

  
  method add_hash (k:string) (n:int)=Hashtbl.add hash k n;Hashtbl.add hash_rev n k
  method replace_hash k n=
    let i=self#get_hash k in
      self#del_hash k;
      self#del_hash_rev i;
      self#add_hash n i;
  method get_hash k=
    (try 
       Hashtbl.find hash k
     with Not_found -> raise (Game_obj_hash_not_found k))

  method del_hash k=
    (try 
       Hashtbl.remove hash k
     with Not_found -> raise (Game_obj_hash_not_found k))
 
  method get_hash_rev n=Hashtbl.find hash_rev n
  method del_hash_rev n=Hashtbl.remove hash_rev n

  method is_hash k=Hashtbl.mem hash k

  method foreach_object_hash f=
    Hashtbl.iter (
      fun k i ->
	f k (self#get_hash_object k)
    ) hash

  method del_hash_object (k:string)=
    let n=self#get_hash k in
    self#del_object n;
    self#del_hash k;
    self#del_hash_rev n

  method get_hash_object (k:string)=
    self#get_object (self#get_hash k)
end;;

class game_object_layer_hash wi hi max=
object(self)
  inherit [game_object] game_obj_layer_hash none_obj wi hi max as super
end;;


class game_generic_object_layer_hash wi hi max=
object(self)
  inherit [game_generic_object] game_obj_layer_hash none_generic_obj wi hi max as super
end;;

class game_generic_object_types=
object
  inherit [game_generic_object] obj_types none_generic_obj
end;;

exception Border_out_of_array of (int*int);;
exception Shaded_out_of_array of (int*int);;

class tile_layer wi hi tw th=
  object (self)
    inherit game_layer wi hi as super
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
