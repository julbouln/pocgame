open Low;;

open Video;;
open Anim;;

open Game_main;;


type loading_data=
  | LNone
  | LData of string
  | LEnd;;

class game_loading_info=
object
  val mutable m=Mutex.create();
  val mutable cond=Condition.create();

  val mutable data=LNone
  method get_data=
    Mutex.lock m;
    Condition.wait cond m;
    let d=data in
      Mutex.unlock m;
      d

  method set_data d=
    Mutex.lock m;
    data<-d;
    Condition.signal cond;
    Mutex.unlock m;
    (* to avoid interblockade *)
    Thread.delay (Random.float (1./. 25.));

end;;

class game_loading (li:game_loading_info) msgt=
object(self)
  val mutable waiting=new graphic_object_anim 67 67 "medias/iface/sablier.png" [|0;1;2;3;4;5|] 4

  method loading()=
    self#on_load();
    let d=ref LNone in
      while (d:=li#get_data; !d <> LEnd) do
	self#on_loop !d;
      done

  method on_load()=
    waiting#move 512 384;


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
	    tile_string (main#screen_tile) (490,460) "Please wait ..." (255,255,255);
	    video#flip();
      | LEnd -> ()
      
end;;
