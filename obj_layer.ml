open Low;;
open Rect;;
open Video;;
open Object;;

open Obj_type;;
open Layer;;

(** Game object layer class definition *)

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


class ['a] obj_layer (none_obj:'a) wi hi max=
  object (self)
    inherit layer wi hi as super
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
