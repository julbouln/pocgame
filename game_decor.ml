open Action;;

open Game_object;;

open Game_xml;;

class game_decor nm w h file cw ch stc=
object (self)
  inherit game_object nm w h file false false cw ch as super

  initializer 
(*    super#set_blocking true; *)
    if stc#is_state "idle" then (
      let idle_state=new state_object "idle" (stc#get_state "idle").frames (stc#get_state "idle").refresh in
	idle_state#set_action (fun()->());
	state_manager#add_state idle_state;
    );
   self#set_state "idle";
  
end;;

class xml_decors_parser=
object
  inherit xml_gm_objects_parser "decor"
end;;
