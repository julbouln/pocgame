open Value_xml;;
open Value_lua;;
open Value_val;;

open Core_val;;
open Core_net;;
open Core_stage;;
open Core_timer;;

open Game_engine;;
open Game_map;;

open Net_message;;
open Net_conn;;
open Net_client;;
open Net_server;;

(** types de messages :
    - add_object : ajoute un objet de jeu
    - delete_object : supprime un objet de jeu
    - set_object_state : change l'etat d'un objet
    - sync_objects : synchronize les objets

    - add_type : ajoute un type d'objet depuis un xml distant
*)



class set_state_object_message_handler is_object set_state=
object(self)
  inherit message_handler
  method parse msg=
    let m=(string_of_val (msg#get_values#get_val (`String "map"))) in
    let mid=(string_of_val (msg#get_values#get_val (`String "map_layer"))) in
    let oid=(string_of_val (msg#get_values#get_val (`String "object"))) in
    let st_id=
      if(msg#get_values#is_val (`String "state")) then
	(Some (string_of_val (msg#get_values#get_val (`String "state"))))
      else None in
    let st_v=new val_ext_handler in
      (match st_id with
	| Some v->
	    st_v#from_xml msg#get_data;
	| None ->());
      if is_object m mid oid then
	set_state m mid oid st_id st_v;

	message_generic_response msg;

  method check msg=
    true
end;;


class add_object_message_handler add_object=
object(self)
  inherit message_handler
  method parse msg=
    let m=(string_of_val (msg#get_values#get_val (`String "map"))) in
    let mid=(string_of_val (msg#get_values#get_val (`String "map_layer"))) in
    let oid=(string_of_val (msg#get_values#get_val (`String "object"))) in
    let otype=(string_of_val (msg#get_values#get_val (`String "type"))) in
    
    let data=new val_ext_handler in
      data#from_xml msg#get_data;
      let (x,y)=(position_of_val (data#get_val (`String "position"))) in
	add_object m mid oid otype x y;
	
	message_generic_response msg;

  method check msg=
    true
end;;


class delete_object_message_handler delete_object=
object(self)
  inherit message_handler
  method parse msg=
    let m=(string_of_val (msg#get_values#get_val (`String "map"))) in
    let mid=(string_of_val (msg#get_values#get_val (`String "map_layer"))) in
    let oid=(string_of_val (msg#get_values#get_val (`String "object"))) in
      delete_object m mid oid;

      message_generic_response msg;

  method check msg=
    true
end;;

class sync_objects_message_handler set_frames from_xml=
object(self)
  inherit message_handler
  method parse msg=
    let m=(string_of_val (msg#get_values#get_val (`String "map"))) in
    from_xml m msg#get_data;

    let fr=(int_of_val (msg#get_values#get_val (`String "frames"))) in
      set_frames fr;
    message_generic_response msg;

  method check msg=
    true
end;;


(** game net stages *)

class net_game_engine curs=
object(self)
  inherit game_engine curs as super

  val mutable nsync=0
  val mutable frames=0
  val mutable tframes=0
  val mutable mframes=0

  method on_loop()=
    super#on_loop();
    frames<-frames+1;
    
  method get_frames()=frames
  method set_frames f=
    if nsync>0 then (
    tframes<-tframes+(f-frames);
    mframes<-tframes/nsync;
    );
    frames<-f;
    nsync<-nsync+1;

    let sffps=30. +. ((float mframes)/.(float 30)) in
    frml#set_ffps sffps;
      
  method init_message_handler (mph:message_parser_handler)=
    mph#handler_add "set_state" (new set_state_object_message_handler 
				   (fun m mid oid->map#is_object mid oid)
				   (fun m mid oid stid stv ->
				      map#set_object_state mid oid stid stv)
				); 
    mph#handler_add "add_object" (new add_object_message_handler 
				    (fun m mid n t x y->
				       ignore(map#add_object_from_type mid (Some n) t x y)
				    )
				 );
    mph#handler_add "delete_object" (new delete_object_message_handler 
				       (fun m mid oid->
					  map#delete_object mid oid
				       )
				    );
    mph#handler_add "sync_objects" (new sync_objects_message_handler 
					 self#set_frames 
					 self#map_from_xml
				   );

  method map_from_xml m xml=
    map#set_xml xml;
    map#xml_of_init();


  method net_lua_message (conn:network_object) dst tp (va:val_generic_handler)=
    va#set_id "values";
    conn#message_send   (xml_message_of_string (
	       "<message type=\""^tp^"\" dst=\""^dst^"\">
                        "^va#to_xml_string^"                        
                       </message>
                      ")
	    );



  method net_set_object_state (conn:network_object) dst m mid n st_id st_v=
    map#set_object_state mid n (Some st_id) st_v;
    st_v#set_id "args";
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"set_state\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"map\" value=\""^m^"\"/>
                         <val_string name=\"map_layer\" value=\""^mid^"\"/>
                         <val_string name=\"object\" value=\""^n^"\"/>
                         <val_string name=\"state\" value=\""^st_id^"\"/>
                        </values>
                        <data>"^st_v#to_xml_string^"
                        </data>
                       </message>
                      ")
      );
    ()




  method net_set_object_no_state (conn:network_object) dst m mid n=
    map#set_object_state mid n (None) (new val_ext_handler);
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"set_state\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"map\" value=\""^m^"\"/>
                         <val_string name=\"map_layer\" value=\""^mid^"\"/>
                         <val_string name=\"object\" value=\""^n^"\"/>
                        </values>
                       </message>
                      ")
      );
    ()

  method net_add_object_named_from_type (conn:network_object) dst m mid n t x y=
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"add_object\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"map\" value=\""^m^"\"/>
                         <val_string name=\"map_layer\" value=\""^mid^"\"/>
                         <val_string name=\"object\" value=\""^n^"\"/>
                         <val_string name=\"type\" value=\""^t^"\"/>
                        </values>
<data>
<args>
 <val_position name=\"position\" x=\""^string_of_int x^"\" y=\""^string_of_int y^"\"/>
</args>
</data>
                       </message>
                      ")
      );
    if map#is_object mid n=false then (
      ignore(map#add_object_from_type mid (Some n) t x y)
    );



  method net_delete_object (conn:network_object) dst m mid n=
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"delete_object\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"map\" value=\""^m^"\"/>
                         <val_string name=\"map_layer\" value=\""^mid^"\"/>
                         <val_string name=\"object\" value=\""^n^"\"/>
                        </values>
                       </message>
                      ")
      );
    map#delete_object mid n


  method net_sync_objects (conn:network_object) dst m=
    map#xml_to_init();
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"sync_objects\" dst=\""^dst^"\">
<values>
                         <val_string name=\"map\" value=\""^m^"\"/>
<val_int name=\"frames\" value=\""^string_of_int frames^"\"/>
</values>
<data>"^
	   map#get_xml#to_string
	 ^"
</data>
                       </message>
                      ")
      );()


  method net_lua_init conn=
    lua#set_val (OLuaVal.String "net_lua_message") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->  OLuaVal.table **->> OLuaVal.unit) 
	 (fun dst tp v->
	    let lo=new lua_obj in
	      lo#from_table v;
	      let vh=new val_generic_handler in
		vh#from_lua lo;


	    self#net_lua_message conn dst tp (vh);()
	 )
      );

    lua#set_val (OLuaVal.String "net_set_object_state") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.unit) 
	 (fun dst m mid id n v->
	    let lo=new lua_obj in
	      lo#from_table v;
	    self#net_set_object_state conn dst m mid id (n) (val_ext_handler_of_format (ValLua lo))
	 )
      );



    lua#set_val (OLuaVal.String "net_set_object_no_state") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#net_set_object_no_state conn));
    lua#set_val (OLuaVal.String "net_add_object") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (self#net_add_object_named_from_type conn));
    lua#set_val (OLuaVal.String "net_delete_object") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#net_delete_object conn));
    lua#set_val (OLuaVal.String "net_sync_objects") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#net_sync_objects conn));

end;;


class net_client_game_engine curs saddr sport cport=
object(self)
  inherit net_game_engine curs as super
  val mutable cli=new network_client cport


  method add_message_handler id h=
    cli#get_mph#handler_add id h


  method on_load()=
    self#init_message_handler cli#get_mph;

    cli#connect saddr sport;
    super#on_load()

  method lua_init()=
    lua#set_val (OLuaVal.String "get_ident") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->cli#get_ident));
    self#net_lua_init (cli:>network_object);

    super#lua_init()  
end;;

(* FIXME : must handle multiple map *)
class net_server_game_engine sport=
object(self)
  inherit net_game_engine generic_cursor as super
  val mutable serv=new network_server sport


(*  initializer
    map#set_canvas None;
*)
  val mutable sync_time=new timer
  method init_sync()=
    sync_time#add_task {h=0;m=0;s=30;f=0} (fun()->self#net_sync_objects (serv:>network_object) "*" "map");
    sync_time#start();
    

  method add_message_handler id h=
    serv#get_mph#handler_add id h

  method on_load()=
    self#init_message_handler serv#get_mph;
    super#on_load();
    self#init_sync();
    let t=Thread.create(function()->serv#run()) () in
      print_string ("Thread "^string_of_int (Thread.id t)^" launched (game_server)");
      print_newline();

  method on_loop()=
    super#on_loop();
    sync_time#step(); 

  val mutable on_connect_fun=fun v->[OLuaVal.Nil]
  method on_connect c=
    ignore(on_connect_fun [OLuaVal.String c])

  val mutable on_disconnect_fun=fun v->[OLuaVal.Nil]
  method on_disconnect c=
    ignore(on_disconnect_fun [OLuaVal.String c])

  method lua_init()=
    lua#set_val (OLuaVal.String "on_connect") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun cli->()));
    lua#set_val (OLuaVal.String "on_disconnect") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun cli->()));
    self#net_lua_init (serv:>network_object); 
    super#lua_init();
    on_connect_fun<-lua#get_fun (OLuaVal.String "on_connect");
    on_disconnect_fun<-lua#get_fun (OLuaVal.String "on_disconnect");
    serv#set_connect self#on_connect;
    serv#set_disconnect self#on_disconnect;

end;;





(* MULTIMAP *)

class net_server_game_world_engine sport=
object(self)
(*  inherit net_game_engine generic_cursor as ge *)
  inherit game_world_engine generic_cursor as super
  val mutable serv=new network_server sport

  val mutable nsync=0
  val mutable frames=0
  val mutable tframes=0
  val mutable mframes=0

  method on_loop()=
    super#on_loop();
    frames<-frames+1;
    
  method get_frames()=frames
  method set_frames f=
    if nsync>0 then (
    tframes<-tframes+(f-frames);
    mframes<-tframes/nsync;
    );
    frames<-f;
    nsync<-nsync+1;

    let sffps=30. +. ((float mframes)/.(float 30)) in
    frml#set_ffps sffps;

  method map_from_xml m xml=
    let nmap=world#get_object m in
      nmap#set_xml xml;
      nmap#xml_of_init();

  method init_message_handler (mph:message_parser_handler)=
    mph#handler_add "set_state" (new set_state_object_message_handler 
				   (fun m mid oid->world#is_map_object m mid oid)
				   (fun m mid oid stid stv ->
				      world#set_object_state m mid oid stid stv)
				); 
    mph#handler_add "add_object" (new add_object_message_handler 
				    (fun m mid n t x y->
				       ignore(world#add_object_from_type m mid (Some n) t x y)
				    )
				 );
    mph#handler_add "delete_object" (new delete_object_message_handler 
				       (fun m mid oid->
					  world#delete_map_object m mid oid
				       )
				    );
    mph#handler_add "sync_objects" (new sync_objects_message_handler 
					 self#set_frames 
					 self#map_from_xml
				   );

(*  initializer
    map#set_canvas None;
*)
  val mutable sync_time=new timer
  method init_sync()=
    sync_time#start();
    
  method add_message_handler id h=
    serv#get_mph#handler_add id h

  method new_map_from_file s f=
    world#new_map_from_file s f;
    serv#get_trans#group_add s [];
    sync_time#add_task {h=0;m=0;s=30;f=0} (fun()->self#net_sync_objects (serv:>network_object) ("#"^s) s);

  method on_load()=
    self#init_message_handler serv#get_mph;
    super#on_load();
    self#init_sync();
    let t=Thread.create(function()->serv#run()) () in
      print_string ("Thread "^string_of_int (Thread.id t)^" launched (game_server)");
      print_newline();

  method on_loop()=
    super#on_loop();
    sync_time#step(); 

  val mutable on_connect_fun=fun v->[OLuaVal.Nil]
  method on_connect c=
    ignore(on_connect_fun [OLuaVal.String c])

  val mutable on_disconnect_fun=fun v->[OLuaVal.Nil]
  method on_disconnect c=
    ignore(on_disconnect_fun [OLuaVal.String c])

  method net_lua_message (conn:network_object) dst tp (va:val_generic_handler)=
    va#set_id "values";
    conn#message_send   (xml_message_of_string (
	       "<message type=\""^tp^"\" dst=\""^dst^"\">
                        "^va#to_xml_string^"                        
                       </message>
                      ")
	    );

  method net_set_object_state (conn:network_object) dst m mid n st_id st_v=
    let nmap=world#get_object m in
    nmap#set_object_state mid n (Some st_id) st_v;
    st_v#set_id "args";
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"set_state\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"map\" value=\""^m^"\"/>
                         <val_string name=\"map_layer\" value=\""^mid^"\"/>
                         <val_string name=\"object\" value=\""^n^"\"/>
                         <val_string name=\"state\" value=\""^st_id^"\"/>
                        </values>
                        <data>"^st_v#to_xml_string^"
                        </data>
                       </message>
                      ")
      );
    ()



  method net_set_object_no_state (conn:network_object) dst m mid n=
    let nmap=world#get_object m in
    nmap#set_object_state mid n (None) (new val_ext_handler);
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"set_state\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"map\" value=\""^m^"\"/>
                         <val_string name=\"map_layer\" value=\""^mid^"\"/>
                         <val_string name=\"object\" value=\""^n^"\"/>
                        </values>
                       </message>
                      ")
      );
    ()

  method net_add_object_named_from_type (conn:network_object) dst m mid n t x y=
    let nmap=world#get_object m in
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"add_object\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"map\" value=\""^m^"\"/>
                         <val_string name=\"map_layer\" value=\""^mid^"\"/>
                         <val_string name=\"object\" value=\""^n^"\"/>
                         <val_string name=\"type\" value=\""^t^"\"/>
                        </values>
<data>
<args>
 <val_position name=\"position\" x=\""^string_of_int x^"\" y=\""^string_of_int y^"\"/>
</args>
</data>
                       </message>
                      ")
      );
    if nmap#is_object mid n=false then (
      nmap#add_object_from_type mid (Some n) t x y;()
    )

  method net_delete_object (conn:network_object) dst m mid n=
    let nmap=world#get_object m in
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"delete_object\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"map\" value=\""^m^"\"/>
                         <val_string name=\"map_layer\" value=\""^mid^"\"/>
                         <val_string name=\"object\" value=\""^n^"\"/>
                        </values>
                       </message>
                      ")
      );
    nmap#delete_object mid n

  method net_sync_objects (conn:network_object) dst m=
    let nmap=world#get_object m in
    nmap#xml_to_init();
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"sync_objects\" dst=\""^dst^"\">
<values>
                         <val_string name=\"map\" value=\""^m^"\"/>
<val_int name=\"frames\" value=\""^string_of_int frames^"\"/>
</values>
<data>"^
	   nmap#get_xml#to_string
	 ^"
</data>
                       </message>
                      ")
      );()

  method net_lua_init conn=
    lua#set_val (OLuaVal.String "net_lua_message") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->  OLuaVal.table **->> OLuaVal.unit) 
	 (fun dst tp v->
	    let lo=new lua_obj in
	      lo#from_table v;
	      let vh=new val_generic_handler in
		vh#from_lua lo;

	    self#net_lua_message conn dst tp (vh);()
	 )
      );

    lua#set_val (OLuaVal.String "net_set_object_state") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.unit) 
	 (fun dst m mid id n v->
	    let lo=new lua_obj in
	      lo#from_table v;
	    self#net_set_object_state conn dst m mid id (n) (val_ext_handler_of_format (ValLua lo))
	 )
      );



    lua#set_val (OLuaVal.String "net_set_object_no_state") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#net_set_object_no_state conn));
    lua#set_val (OLuaVal.String "net_add_object") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (self#net_add_object_named_from_type conn));
    lua#set_val (OLuaVal.String "net_delete_object") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#net_delete_object conn));
    lua#set_val (OLuaVal.String "net_sync_objects") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#net_sync_objects conn));



  method lua_init()=
   lua#set_val (OLuaVal.String "new_map_from_file") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) self#new_map_from_file);

    lua#set_val (OLuaVal.String "group_add_dest") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) serv#get_trans#group_add_dest);
    lua#set_val (OLuaVal.String "group_del_dest") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) serv#get_trans#group_del_dest);

    lua#set_val (OLuaVal.String "on_connect") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun cli->()));
    lua#set_val (OLuaVal.String "on_disconnect") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun cli->()));
    self#net_lua_init (serv:>network_object); 
    super#lua_init();
    on_connect_fun<-lua#get_fun (OLuaVal.String "on_connect");
    on_disconnect_fun<-lua#get_fun (OLuaVal.String "on_disconnect");
    serv#set_connect self#on_connect;
    serv#set_disconnect self#on_disconnect;

end;;
