open Game_object;;
open Oxml;;

class game_tree_node (i:string)=
object
  val mutable id=i
  method get_id=id
  val mutable props=new game_properties
  method set_props p=props<-p
  method get_props=props
end;;

type game_tree_t=
  | Node of game_tree_node * game_tree_t DynArray.t
  | NodeEmpty;;

class game_tree=
object(self)
  val mutable tree=NodeEmpty
  
  method init_tree n=tree<-n

  method get_node nm=
    let r=ref NodeEmpty in
    self#traverse (
      fun p n->
	match n with
	  | Node (no,tl)->
	      if no#get_id=nm then
		r:=Node (no,tl)
	  | NodeEmpty -> ()	
    );
      !r

  method add_child parent (ch:game_tree_t)=
    self#traverse (
      fun p n->
	match n with
	  | Node (no,tl)->
	      if no#get_id=parent then
		DynArray.add tl n
	  | NodeEmpty -> ()	
    )


  method traverse (fu:string -> game_tree_t -> unit)=
    let rec p (parent:string) (n:game_tree_t) (f:string->game_tree_t->unit)=      
      (match n with
	 | Node (o,nl) -> 
	     (f parent n);
	     DynArray.iter (fun cn->p o#get_id cn f) (nl);	     
	 | NodeEmpty ->  ());
    in
      p "root" tree fu


end;;

let empty_tree_node()=
  let a=DynArray.create() in
    DynArray.add a NodeEmpty;a



class xml_tree_node_parser=
object
  inherit xml_parser as super
  val mutable id=""
  val mutable parent=""
  val mutable props=new game_properties

  method get_val=let n=new game_tree_node id in
    n#set_props props; (parent,n)
    

  method parse_attr k v=
    match k with
      | "id" -> id<-v
      | _ -> ()


  method parse_child k v=
    match k with
      | "parent" -> let p=(new xml_string_parser "id") in p#parse v;parent<-p#get_val
      | "properties" -> let p=(new xml_prop_list_parser) in p#parse v;
	  List.iter (fun (id,p)->props#add_prop id p) p#get_list
      | _ -> ()
end;;


class ['a] xml_tree_parser t f=
object(self)
  inherit ['a] xml_list_parser t f

  method get_val=
    let t=new game_tree in
      List.iter (fun (p,n)->
		   t#add_child p (Node (n,empty_tree_node()))
		) self#get_list;
      t
end;;
