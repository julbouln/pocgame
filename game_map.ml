open Value_common;;
open Value_lua;;
open Value_xml;;
open Value_xinclude;;
open Value_val;;

open Core_val;;
open Core_medias;;
open Core_graphic;;
open Core_rect;;
open Core_file;;
open Core_action;;
open Core_xml;;
open Core_type;;

open Game_xml;;
open Game_object;;
open Game_object_layer;;
open Game_tile_layer;;
open Game_decor;;
open Game_xml;;

open Game_loading;;



(** game_object types *)
class game_object_types=
object(self)
  inherit [game_object] obj_types
end;;

class game_object_map wi hi=
object(self)
  inherit generic_object
  inherit lua_object as lo
  inherit [game_object] game_obj_layer wi hi as super

  val mutable canvas=None
  method set_canvas (c:canvas option)=canvas<-c

  method is_obj_with_type t=
    let r=ref false in
      self#foreach_object
	( fun i o->
	    if o#get_name=t then r:=true
	);
      !r
	
  val mutable obj_type=new game_object_types
  method get_obj_type=obj_type
    
  method add_object_type nm (t:unit->'a)=
    obj_type#add_object_type nm t
  method get_object_from_type nm=
    obj_type#get_object_type nm




  (** general *)
  method add_object_at (id:string option) (o:'a) (x:int) (y:int)=    

    o#move x y;
    let n=self#add_object id o in
(*      print_string ("GAME_OBJECT_MAP: add object "^n);print_newline(); *)
(*      o#lua_init();*)
      self#lua_parent_of n (o:>lua_object);
      n
	  
  method update()=
    self#clear();
    self#update_obj_all();
    self#update_action(); 


  method add_object_from_type id t x y=
    let o=self#get_object_from_type t in
      o#set_name t;
      (match canvas with 
	 | Some cvas->o#graphics_register cvas#add_obj;
	 | None -> ());
      self#add_object_at id o x y 

  method delete_object id=
    let o=self#get_object id in
      (match canvas with 
	 | Some cvas->o#graphics_unregister cvas#del_obj;
	 | None -> ());

      lo#get_lua#del_val (OLuaVal.String id) ;
      super#delete_object id;

  method copy_object cid id=
    let o=self#get_object id in
    let no=(self#get_object_from_type o#get_name) in
    self#add_object_at cid no o#get_rect#get_x o#get_rect#get_y


  method to_xml_string=
    let x=self#to_xml in
      Xml.to_string x

  method to_xml=
    Xml.Element 
      ("game_object_map",[("id",self#get_id)],
	 let a=DynArray.create() in
	   self#foreach_object (
	     fun k o->
	       let e=
		 Xml.Element 
		   ("game_object",[("id",k);("type",o#get_name)],[
		      (* args *)
		      (
			let vh=new val_ext_handler in
			 vh#set_val (`String "position") (`Position (o#get_rect#get_x,o#get_rect#get_y));
			 vh#set_id "args";
			 let n=vh#to_xml in
			   n#to_xml_t
		      );
		      (* properties *)
		      (
			let pr=(o#get_props) in
			 let n=pr#to_xml in
			   n#to_xml_t
				   

		      )		      
		    ]) in
		 DynArray.add a e;
	   );
	   DynArray.to_list a
       );


  method from_xml x=
    match x with
      | Xml.Element(t,attr,childs)->
	 List.iter (
	   fun c->
	     match c with
	       | Xml.Element(ct,cattr,cchilds)->
		   let args=new val_ext_handler and
		       props=new val_ext_handler in
		     List.iter (
		       fun cc->
			 match (Xml.tag cc) with
			 | "args" -> 
			     let n=new xml_node in
			       n#of_xml_t cc;
			       args#from_xml n
			 | "properties" -> 
			     let n=new xml_node in
			       n#of_xml_t cc;
			       props#from_xml n
			 | _ ->()
		     ) cchilds;
		     let (x,y)=position_of_val (args#get_val (`String "position")) in
		     let oid=self#add_object_from_type (Some (Xml.attrib c "id")) (Xml.attrib c "type") x y in
		     let o=self#get_object oid in
		       o#get_props#flatten props;
	       | _ ->()

	 ) childs;
	    
      | _ ->()

    

 method lua_init()=
   lua#set_val (OLuaVal.String "add_object_from_type") 
     (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.string) 
	(fun t x y->self#add_object_from_type None t x y));
   lua#set_val (OLuaVal.String "add_object_named_from_type") 
     (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) 
	(fun n t x y->ignore(self#add_object_from_type (Some n) t x y)));
   
   lua#set_val (OLuaVal.String "delete_object") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#delete_object);


    lua#set_val (OLuaVal.String "get_object_id_at_position") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.value) 
	 (fun x y->
	    let n=self#get_position x y in
	      match n with 
		| Some i -> OLuaVal.String i
		| None -> OLuaVal.Nil
	 )
      );

    lua#set_val (OLuaVal.String "get_object_at_position") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.value) 
	 (fun x y->
	    let n=self#get_position x y in
	      match n with 
		| Some i -> (lua#get_val (OLuaVal.String i))
		| None -> OLuaVal.Nil
	 )
      );

    self#lua_parent_of "types" (obj_type:>lua_object); 



   lo#lua_init();

end;;



class game_tile_layer_handler=
object
  inherit [game_generic_tile_layer] generic_object_handler
end;;

class game_object_map_handler=
object
  inherit [game_object_map] generic_object_handler
end;;


(* xml type parser *)

(* game_object_map *)

class xml_game_object_map_type_parser=
object(self)
  inherit [game_object_map] xml_object_parser (fun()->new game_object_map 0 0) as super
  val mutable obj_types_parser=new xml_game_object_types_parser

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "game_object_types" -> obj_types_parser#parse v
      | _ ->()

  method init_object o=
    o#set_lua_script lua;

    let h=obj_types_parser#get_hash in
      Hashtbl.iter (
	fun k v ->
	  o#get_obj_type#add_object_type k v;
      ) h;
(*
    o#init_object_types_from_xml (string_of_val (args_parser#get_val#get_val (`String "types")))
*)

end;;



class xml_game_object_maps_type_parser=
object(self)
  inherit [xml_game_object_map_type_parser,game_object_map] xml_container_parser "game_object_map_type" (fun()->new xml_game_object_map_type_parser) as super

  initializer
    self#parser_add "with_loading" (fun()->new xml_game_object_map_type_parser);
    self#parser_add "normal" (fun()->new xml_game_object_map_type_parser);
    



end;;

(* game_tile_layer *)

class xml_game_tile_layer_type_parser=
object(self)
  inherit [game_generic_tile_layer] xml_object_parser (fun()->new game_generic_tile_layer 0 0 32 32) as super

  method get_type="unique"


  method init_object o=
    o#set_lua_script lua;


  method get_val=
    let ofun()=
      let o=
	new game_tile_layer 0 0 32 32 (string_of_val (args_parser#get_val#get_val (`String "file")))
      in
	self#init_object (o:>game_generic_tile_layer);
	(o:>game_generic_tile_layer)	  
    in      
      (id,ofun)


end;;

class xml_game_tile_layers_type_parser=
object(self)
  inherit [xml_game_tile_layer_type_parser,game_generic_tile_layer] xml_container_parser "game_tile_layer_type" (fun()->new xml_game_tile_layer_type_parser)

  initializer
    self#parser_add "unique" (fun()->new xml_game_tile_layer_type_parser)
end;;


class xml_game_map_type_parser=
object(self)
  inherit xml_parser

  val mutable maps_type_parser=new xml_game_object_maps_type_parser
  val mutable layers_type_parser=new xml_game_tile_layers_type_parser

 
  method parse_attr k v=()

  method parse_child k v=
    match k with
      | "game_object_maps_type" ->
	  maps_type_parser#parse v;	  
	  
      | "game_tile_layers_type" ->
	  layers_type_parser#parse v;
      | _ -> ()

  method init add_map add_lay=
    maps_type_parser#init_simple (add_map);
    layers_type_parser#init_simple (add_lay);

end;;

let init_game_map_type_from_xml f add_map add_lay=
(*  let obj_xml=new xml_node (Xml.parse_file f) in *)
  let obj_xml=xml_node_from_file f in
  let pmt=new xml_game_map_type_parser in
    pmt#parse obj_xml;
    pmt#init add_map add_lay;;


(* game map *)

class game_map w h=
object(self)
  inherit lua_object as lo
  
  val mutable actions=new state_object

  val mutable canvas=None
  method set_canvas (c:canvas option)=canvas<-c

  val mutable rect=new rectangle 0 0 w h
  method get_rect=rect

  method resize nw nh=
    self#foreach_tile_layer 
      ( 
	fun k t->
	  t#resize nw nh
      );
    self#foreach_object_map 
      ( 
	fun k om->
	  om#resize nw nh
      );
    rect#set_size nw nh;

  (* tile layers *)
  val mutable tile_layers=new game_tile_layer_handler

  method add_tile_layer (s:string) (o:game_generic_tile_layer)=
(*    print_string "add tile layer";print_newline(); *)
    ignore(tile_layers#add_object (Some s) o)

  method get_tile_layer (s:string)=
    tile_layers#get_object s

  method foreach_tile_layer f=
    tile_layers#foreach_object f

  method tile_layer_init tn t=
    let tl=self#get_tile_layer tn in
    for i=0 to self#get_rect#get_w-1 do
      for j=0 to self#get_rect#get_h-1 do
	let mt=randomize 2 in
	  tl#set_position i j (Some (t+mt)); 
      done;
    done;


  (* object maps *)
  val mutable object_maps=new game_object_map_handler
    
  method add_object_map (s:string) (o:game_object_map)=
(*    print_string "add object map";print_newline(); *)
(*    ignore(o#lua_init()); *)
    self#lua_parent_of s (o:>lua_object);
    o#set_canvas canvas;
    ignore(object_maps#add_object (Some s) o);
    
  method get_object_map (s:string)=
    object_maps#get_object s

  method foreach_object_map f=
    object_maps#foreach_object f


  method is_position_blocking x y=
    if rect#is_position x y then
      (
	let r=ref false in
	  self#foreach_object_map (
	    fun mid m->
	      if m#is_object_at_position x y then
	      let o=m#get_object_at_position x y in
		r:=!r || o#get_blocking
	  );
	  !r
      )
    else
      true
(*    if self#get_tile_layer#out_of_lay x y then true else false
*)
(*
    else
      if self#get_decor_map#is_object x y then
	let o=self#get_decor_map#get_object_by_position x y in
	  o#get_blocking
      else false
*)

(* objects *)

  method add_object_from_type mid id t x y=
    let m=self#get_object_map mid in
    let n=m#add_object_from_type id t x y in
      n

  method add_object_named_from_type mid id t x y=
    let m=self#get_object_map mid in
      ignore(m#add_object_from_type (Some id) t x y);
	
  method copy_object mid=
    let m=self#get_object_map mid in
      m#copy_object

  method move_object mid=
    let m=self#get_object_map mid in
      m#move_object

  method delete_object mid id=
    let m=self#get_object_map mid in
      m#delete_object id

  method is_object mid=
    let m=self#get_object_map mid in
      m#is_object

  method get_object mid=
    let m=self#get_object_map mid in
      m#get_object

  method get_object_id_at_position mid x y=
    let m=self#get_object_map mid in
    m#get_position x y

  method foreach_object mid f=
    let m=self#get_object_map mid in
      m#foreach_object f

(* update layer *)

  method update()=
    self#foreach_object_map (fun i m->m#update());
    actions#loop();

  method save_to_file f=
    let fo=open_out f in
      output_string fo (self#to_xml_string);
      close_out fo;
 
  method to_xml_string=
    let x=self#to_xml in
      Xml.to_string x

  method to_xml=
    Xml.Element("game_map",[("w",string_of_int self#get_rect#get_w);
			    ("h",string_of_int self#get_rect#get_h)],
		[
		  Xml.Element("game_object_maps",[],
			      let a=DynArray.create() in
				self#foreach_object_map(
				  fun k m->
				    DynArray.add a m#to_xml
				);
				DynArray.to_list a
			     );
		  Xml.Element("game_tile_layers",[],
			      let a=DynArray.create() in
				self#foreach_tile_layer (
				  fun k m->
				    DynArray.add a m#to_xml
				);
				DynArray.to_list a
			     )
		]
	       );

  method load_from_file f=
    let xinc=xinclude_process_file f in
    let x=Xml.parse_string xinc in
      self#from_xml x
		
  method from_xml_string s=
    let x=Xml.parse_string s in
      self#from_xml x

  method from_xml x=
    match x with
      | Xml.Element(t,attr,childs)->
	  let w=int_of_string(Xml.attrib x "w") and
	      h=int_of_string(Xml.attrib x "h") in
	    self#resize w h;
	    List.iter(
	      fun c->
		match (Xml.tag c) with
		  | "game_object_maps" ->
		      List.iter (
			fun oc->
			  let om=self#get_object_map (Xml.attrib oc "id") in
			    om#from_xml oc;
		      ) (Xml.children c);
		  | "game_tile_layers" ->
		      List.iter (
			fun oc->
			  let ot=self#get_tile_layer (Xml.attrib oc "id") in
			    ot#from_xml oc;
		      ) (Xml.children c);
		      
		  | "actions" ->
		      let n=new xml_node in
			n#of_xml_t c;
		      let p=(Global.get xml_default_actions_parser)() in
			p#parse n;
			p#init_simple (actions#add_action);
		  | _ -> ()
	    ) childs;
      | _ -> ()



  method lua_init()=
(* DEPRECATED use mapname.func instead *)
    lua#set_val (OLuaVal.String "add_object_from_type") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.string) (fun m t x y->self#add_object_from_type m None t x y));
    lua#set_val (OLuaVal.String "add_object_named_from_type") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#add_object_named_from_type);
    lua#set_val (OLuaVal.String "delete_object") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) self#delete_object);
    lua#set_val (OLuaVal.String "get_object_id_at_position") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.value) 
	 (fun m x y->
	    let n=self#get_object_id_at_position m x y in
	      match n with 
		| Some i -> OLuaVal.String i
		| None -> OLuaVal.Nil
	 )
      );
(* /DEPRECATED *)



    lua#set_val (OLuaVal.String "is_position_blocking") 
      (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.bool) 
	 self#is_position_blocking
      );


    lua#set_val (OLuaVal.String "init_tile_layer") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.int **->> OLuaVal.unit) self#tile_layer_init);

    lua#set_val (OLuaVal.String "resize") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#resize);

    lua#set_val (OLuaVal.String "load_from_file") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#load_from_file);
    lua#set_val (OLuaVal.String "save_to_file") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#save_to_file);


    actions#lua_init();
    self#lua_parent_of "actions" (actions:>lua_object);

    lo#lua_init();

end;;

