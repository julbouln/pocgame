open Low;;

open Video;;
open Anim;;

open Medias;;

open Main;;


type loading_data=
  | LNone
  | LData of string
  | LEnd;;

class game_loading_info=
object
  val mutable m=Mutex.create();
  val mutable cond=Condition.create();

  val mutable data=LNone

  method get_lock()=
    while (Mutex.try_lock m=false) do
      Thread.delay (Random.float (0.15))
    done;

    Condition.wait cond m;
 
  method get_unlock()=
    Mutex.unlock m;    

  method get_data=
    let d=data in
      d

  method set_lock()=
    while (Mutex.try_lock m=false) do
      Thread.delay (Random.float (0.15))
    done;

  method set_unlock()=
    Condition.signal cond;
    Mutex.unlock m;

    (* to avoid interblockade *)
    Thread.delay (Random.float (0.15)); 

  method set_data d=
    data<-d;

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
    self#on_load();
    let d=ref LNone in
      while (li#get_lock();d:=li#get_data; !d <> LEnd) do
	
	self#on_loop !d;
	li#get_unlock();
      done;
      li#get_unlock();

  method on_load()=
    waiting#move (video#f_size_w 512) (video#f_size_h 384);


  method on_loop msg1=
    video#blank();
    match msg1 with
      | LNone ->()
      | LData m ->
	  let msg=(msgt^" "^ m ^"...")  in
	    tile_string (main#screen_tile) (0,0) msg (255,255,255);
	    print_string msg;print_newline();
      
	    waiting#anim();
	    waiting#put();

	    tile_string (main#screen_tile) ((video#f_size_w 490),(video#f_size_h 460)) "Please wait ..." (255,255,255);
	    video#flip();
      | LEnd -> ();
        
end;;
