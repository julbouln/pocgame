open Core_event;;
open Core_main;;
open Core_video;;
open Core_stage;;
open Core_xml;;
open Game_engine;;
open Game_xml;;



let extract_entry ifile e =
(*  print_string e.Zip.filename; print_newline(); *)
  if e.Zip.is_directory then begin
    try
      Unix.mkdir e.Zip.filename 0o777
    with Unix.Unix_error(Unix.EEXIST, _, _) -> ()
  end else begin
    Zip.copy_entry_to_file ifile e e.Zip.filename
  end;;

let extract zipfile =
  let ic = Zip.open_in zipfile in
  let es=(Zip.entries ic) in
  let pdir=List.nth es 0 in
    List.iter (extract_entry ic) es;
    Zip.close_in ic;
    pdir.Zip.filename;;


Global.set xml_default_stages_parser xml_engine_stages_parser;;

let narg=Array.length (Sys.argv);;

if narg>1 then (
  let file=(Sys.argv).(narg-1) in
    if Sys.file_exists file then (
      if Filename.check_suffix file ".xml" then
	game_init_from_xml file 
      else if Filename.check_suffix file ".xpoc" then (
	let pdir=extract file in
	  Sys.chdir pdir;
(*	  at_exit(fun()->
		    Sys.chdir "../";
		    Unix.rmdir pdir );
*)
	  game_init_from_xml "xpoc.xml"
      )
      else
	(
	  print_string ("pocengine: file "^file^" not xml or XPOC!");print_newline()
	)
    )
    else (
      print_string ("pocengine: file "^file^" not found!");print_newline()
    )
)
else (
  print_string ("pocengine: you must specify a xml or XPOC file!");print_newline()
)
