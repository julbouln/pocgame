open Oxml;;

open Obj_type;;
open Game_obj;;
type gm_object=
{
 ofile:string;
(* decal value *)
 odw:int;
 odh:int;
(* pixel *)
 ow:int;
 oh:int;
 oframes:int list;
 orefresh:int;
 ocw:int;
 och:int;
};;

class xml_gm_object_parser=
object
  inherit xml_parser

  val mutable file=""
  val mutable w=0
  val mutable h=0
  val mutable dw=0
  val mutable dh=0
  val mutable cw=0
  val mutable ch=0
  val mutable frames=[]
  val mutable refresh=0

  method get_val={ofile=file;odw=dw;odh=dh;ow=w;oh=h;oframes=frames;orefresh=refresh;ocw=cw;och=ch}

  method tag=""
  method parse_attr k v=()
  method parse_child k v=
    match k with
      | "file" -> let p=(new xml_string_parser "path") in p#parse v;file<-p#get_val    
      | "pixel_size" -> let p=(new xml_size_parser ) in p#parse v;w<-p#get_w;h<-p#get_h;
      | "case_size" -> let p=(new xml_size_parser ) in p#parse v;cw<-p#get_w;ch<-p#get_h;
      | "decal_value" -> let p=(new xml_size_parser ) in p#parse v;dw<-p#get_w;dh<-p#get_h;
      | "frames" -> let p=(new xml_intlist_parser "frame" (fun()->new xml_int_parser "frame"))  in p#parse v;frames<-p#get_list
      | "refresh" -> let p=(new xml_int_parser "value") in p#parse v;refresh<-p#get_val
      | _ -> ()

end;;

class xml_gm_objects_parser name=
object
  inherit [gm_object] xml_list_parser name (fun()->new xml_gm_object_parser)
end;;
