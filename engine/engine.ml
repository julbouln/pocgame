open Event;;
open Main;;
open Video;;
open Stage;;
open Game_engine;;
open Core_xml;;

Global.set xml_default_stages_parser xml_engine_stages_parser;;

let narg=Array.length (Sys.argv);;
game_init_from_xml (Sys.argv).(narg-1)

