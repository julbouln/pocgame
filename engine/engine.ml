open Core_event;;
open Core_main;;
open Core_video;;
open Core_stage;;
open Core_xml;;
open Game_engine;;
open Game_xml;;

Global.set xml_default_stages_parser xml_engine_stages_parser;;

let narg=Array.length (Sys.argv);;

if narg>1 then (
  let file=(Sys.argv).(narg-1) in
    if Sys.file_exists file then
      game_init_from_xml file
    else (
      print_string ("pocengine: XPOC file "^file^" not found!");print_newline()
    )
)
else (
  print_string ("pocengine: you must specify a XPOC file!");print_newline()
)
