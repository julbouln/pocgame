open Action;;

open Game_obj;;

open Game_xml;;

class game_decor nm w h file cw ch fr r=
object (self)
  inherit game_object nm w h file false false cw ch as super
  val mutable ds=new state_object "idle" fr r 

  initializer 
    super#set_blocking true;
    state_manager#add_state ds;
    self#set_state "idle";
  
end;;

class xml_decors_parser=
object
  inherit xml_gm_objects_parser "decor"
end;;
