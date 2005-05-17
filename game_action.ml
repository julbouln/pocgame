
open Core_val;;
open Core_fun;;
open Core_action;;


class action_game=
object
  inherit action_lua
  method private get_object=
    let obj=fnode#get_parent#get_parent#get_parent in
      game_object_of_fun obj#get_fun
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
    current<-(self#get_object#case_x(),self#get_object#case_y());
    dest<-position_of_val (ve#get_val (`Int 0));
    (*
      path<-self#get_map#path_calc current dest;
    *)
    super#on_start ve

  method on_loop()=
    current<-(self#get_object#case_x(),self#get_object#case_y());
    let (dx,dy)=dest in
    let (cx,cy)=current in
      if dx<>cx || dy<>cy then
	(
	  let (nx,ny)=path.(cur_path+1) in
	    match (nx-cx,ny-cy) with
	      | (-1,-1) -> ()
	      | _ -> ()

	);
      super#on_loop()

end;;
