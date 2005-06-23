
open Core_val;;
open Core_fun;;
open Core_action;;


class action_game=
object
  inherit action_lua
  method private get_object=
    let obj=fnode#get_parent#get_parent#get_parent in
      game_object_of_fun obj#get_fun

  method private get_object_map=
    let obj=fnode#get_parent#get_parent#get_parent#get_parent in
      game_object_map_of_fun obj#get_fun

  method private get_map=
    let obj=fnode#get_parent#get_parent#get_parent#get_parent#get_parent in
      game_map_of_fun obj#get_fun
end;;


class action_pathfinding=
object(self)
  inherit action_game as super

  val mutable vel=2

  val mutable current=(0,0)
  val mutable dest=(0,0)

  val mutable path=Array.create 1 (0,0)
  val mutable cur_path=0

  method on_start ve=
    cur_path<-0;
    current<-(self#get_object#case_x(),self#get_object#case_y());
    dest<-position_of_val (ve#get_val (`Int 0));
    
    path<-self#get_map#path_calc current dest;
    
    super#on_start ve

  val mutable cpx=0
  val mutable cpy=0		    

  method on_loop()=
    let (nx,ny)=
      if cur_path+1<Array.length path-1 then
	path.(cur_path+1)
      else
	dest
    in
    current<-(self#get_object#case_x(),self#get_object#case_y());
    let (dx,dy)=dest in
    let (cx,cy)=current in
      if dx<>cx || dy<>cy then
	(

	    cpx<-self#get_sprite#get_x();
	    cpy<-self#get_sprite#get_y();
	    
	    cpx<-cpx+ ((nx-cx)*vel);
	    cpy<-cpy+ ((ny-cy)*vel);

	    self#get_sprite#jump cpx cpy;

	    (match (nx-cx,ny-cy) with
	      | (-1,-1)->self#get_object#turn 7
	      | (-1,0)->self#get_object#turn 6
	      | (-1,1)->self#get_object#turn 5
	      | (0,-1)->self#get_object#turn 0
	      | (0,1)->self#get_object#turn 4
	      | (1,-1)->self#get_object#turn 1
	      | (1,0)->self#get_object#turn 2
	      | (1,1)->self#get_object#turn 3
	      | _ -> ());


	    if nx=cx && ny=cy then 
	      cur_path<-cur_path+1;
	    

	)
      else
	(
	  cur_path<-0;
	);




      super#on_loop()

end;;
