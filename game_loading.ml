open Video;;
open Anim;;

open Medias;;

open Main;;

let rec usleep sec = ignore (Unix.select [] [] [] sec);;

type loading_data=
  | LNone
  | LData of string
  | LEnd;;

let print_loading_data_t d=
  (match d with
    | LNone -> print_string "LNone";
    | LData d-> print_string "LData";
    | LEnd ->print_string "LEnd";
  );print_newline();;

class game_loading_info=
object
  val mutable m=Mutex.create();
  val mutable cond=Condition.create();

  val mutable odata=LData("bla")
  val mutable data=LNone
    
  method get_lock()=
(*      print_string "GAME_LOADING: get_lock()";print_newline(); *)
    Mutex.lock m;
(*    if data=odata then (  *)
(*      print_string "GAME_LOADING: Attente de changement de valeur...";print_newline(); *)
      Condition.wait cond m; 
(*      print_string "GAME_LOADING: Changement de valeur signalé!";print_newline();
*)
(*    )    *)

  method get_unlock()=
(*      print_string "GAME_LOADING: get_unlock()";print_newline(); *)
    Mutex.unlock m;    

  method set_lock()=
(*      print_string "GAME_LOADING: set_lock()";print_newline(); *)
    Mutex.lock m;

  method set_unlock()=
(*      print_string "GAME_LOADING: set_unlock()";print_newline();*)
    Mutex.unlock m;

    (* to avoid interblockade *)
    Thread.delay (Random.float (0.2));

  method get_data=

(*    print_string "GAME_LOADING: Recuperation de valeur";print_newline();*)
    odata<-data;
    data

  method set_data (d:loading_data)=
(*    print_string "GAME_LOADING: Initialisation de valeur...";print_newline();*)
    odata<-data;
    data<-d; 
    Condition.signal cond; 
(*    print_string "GAME_LOADING: Initialisé!";print_newline();*)

end;;

class game_loading=
object(self)
(*  val mutable waiting=new graphic_with_anim (let gr=new graphic_white_border "medias/iface/sablier.png" 67 67 in gr#set_over true; gr:>graphic_generic_object) [|0;1;2;3;4;5|] 4
*)
  val mutable li=new game_loading_info
  method set_loading_info i=li<-i

  val mutable msgt="Empty"
  method set_msgt m=msgt<-m

  val mutable waiting=
    if Sys.file_exists "medias/iface/sablier.png" then
      new graphic_object_anim 67 67 "medias/iface/sablier.png" [|0;1;2;3;4;5|] 4
    else
      new graphic_generic_object_anim "none" [|0;1;2;3;4;5|] 4


  method loading()=
    print_string "GAME_LOADING: Demarrage ...";print_newline();
    self#on_load();
    let d=ref LNone in
      while (!d<>LEnd) do
	li#get_lock();
	d:=li#get_data;
	self#on_loop !d;
	li#get_unlock();
      done;

      print_loading_data_t !d;
    print_string "GAME_LOADING: Fin ...";print_newline();

  method on_load()=
    waiting#move (video#get_w/2 - waiting#get_rect#get_w/2) (video#get_h/2 - waiting#get_rect#get_h/2);


  method on_loop msg1=
    video#blank();
    match msg1 with
      | LNone ->()
      | LData m ->
	  let msg=(msgt^" "^ m ^"...")  in
(*	    tile_string (main#screen_tile) (0,0) msg (255,255,255);*)
	    print_string msg;print_newline();
      
	    waiting#anim();
	    waiting#put();

(*	    tile_string (main#screen_tile) ((video#f_size_w 490),(video#f_size_h 460)) "Please wait ..." (255,255,255);*)
	    video#flip();
      | LEnd -> ();
        
end;;
