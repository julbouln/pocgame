(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Core_rect;;
open Game_dijkstra;;

(** Pathfinding class *)

(* FOR BFR *)

let timeit s f=    
  let ti1=Unix.gettimeofday() in
  let r=f() in
  let tf1=Unix.gettimeofday() in
  print_string (s^" : "); print_float (tf1-.ti1);print_newline();r;;

class pathfinding_map wi hi=
object(self)
  val mutable map=Array.create_matrix wi hi true
  val mutable graph=cree_graphe (-1,-1) (wi*hi)
  val mutable cached_graph=create_rech_graphe (cree_graphe (-1,-1) (wi*hi))

  val mutable rect=new rectangle 0 0 wi hi

  method set_map m=map<-m
  method get_map=map

  method set_position x y v=map.(x).(y)<-v

  method private map2sommet()=
    let a=Array.create (rect#get_w*rect#get_h) (-1,-1) in
    let ca=ref 0 in
      Array.iteri 
	(fun i v->
	   Array.iteri 
	   (fun j w->
(*	      if i>0 || j>0 then *)
(
		a.(!ca)<- (i,j);
		ca:= !ca+1;
	       )
	   ) v
	) map;
      a
	
	
  method private map2arc()=
    let a=Array.create (rect#get_w*rect#get_h*16) ((-1,-1),(-1,-1),0.) in  
    let ca=ref 0 in
      Array.iteri 
	(fun i v->
(*	   if i mod 2=0 then *)
	   Array.iteri 
	   (fun j w->
(*	      if j mod 2=0 then *)
	      if w=true then 
		for k=(-1) to 1 do
		  for l=(-1) to 1 do
		    if (i+k)>=0 && (j+l)>=0 && (i+k)<rect#get_w && (j+l)<rect#get_h then (
		      if map.(i+k).(j+l)=true then (
			let c=ref 1.41 in
			if (k=0 & l=(-1)) || (k=1 & l=0) || (k=0 & l=1) || (k=(-1) & l=0) then 
			  c:=1.;
(*			  print_int !ca;print_newline(); *)
			a.(!ca)<-((i,j),(i+k,j+l),!c);
			ca:= !ca+1;

		       )
		     )
		  done
		done;
	      
	   ) v
	) map;
      a	

  method init()=
    timeit "calcul des sommets" 
      (fun()->Array.iter 
	 (fun x -> ajoute_sommet x graph) 
	 (self#map2sommet())
      );
    timeit "calcul des arcs" 
      (fun()->Array.iter 
	 (fun (a,b,c) -> 
	    let (x1,y1)=a and (x2,y2)=b in 
	      if x1<>(-1)&&y1<>(-1)&&x2<>(-1)&&y2<>(-1) then 
		ajoute_arc a b c graph) 
	 (self#map2arc())
      );
    
    for i=0 to graph.ind -1 do graph.m.(i).(i) <- Cout 0.0 done;
      timeit "calcul des angles adjacents" (fun()->sadj_init graph);
(*      cached_graph<-(timeit "creation du graph" (fun()->create_rech_graphe graph)); *)

  method init_empty w h=
    map<-Array.create_matrix w h true;
    graph<-cree_graphe (-1,-1) (w*h);
    cached_graph<-create_rech_graphe (cree_graphe (-1,-1) (w*h));
    rect#set_size w h;

  method init_from_array a=
    self#set_map a;
    self#init();
(*
    self#path_calc_all()    
*)	


  method unblock_position i j=
(*    Printf.printf "unblock %i-%i\n" i j; *)
    map.(i).(j)<-true;

    for k=(-1) to 1 do
      for l=(-1) to 1 do
	if (i+k)>=0 && (j+l)>=0 &&(i+k)<rect#get_w && (j+l)<rect#get_h then
	  if map.(i+k).(j+l)=true then 
	    let c=ref 1.41 in
	    if (k=0 & l=(-1)) || (k=1 & l=0) || (k=0 & l=1) || (k=(-1) & l=0) then c:=1.;
	    ajoute_arc (i+k,j+l) (i,j) !c graph;
      done
    done


  method block_position i j=
(*    Printf.printf "block %i-%i\n" i j; *)
    map.(i).(j)<-false;

    for k=(-1) to 1 do
      for l=(-1) to 1 do
	if (i+k)>=0 && (j+l)>=0 &&(i+k)<rect#get_w && (j+l)<rect#get_h then
(*	  if map.(i+k).(j+l)=true then *)
	    del_arc (i+k,j+l) (i,j) graph;
      done
    done


  method path_calc_all()=
    Array.iter (
      fun v->
	print_string "PATHFINDING : calculate ";
	print_int (fst v);print_string "-";print_int (snd v);print_newline();
	let et=dij_rapide v cached_graph in ()
    ) graph.sommets;


  method path_calc (src:int*int) (dest:int*int) = 
(*    let et = dij_rapide src cached_graph in *)
    let et = dij src graph in 
    let path=DynArray.create() in
      
      if appartient dest graph then (
	let d = index dest graph in
	let rec aux is = 
	  if is = et.source then DynArray.add path graph.sommets.(is)
	  else (
            let old = et.chemins.(is) in 
              aux old;
	      DynArray.add path graph.sommets.(is)
	  )
	in 
	  if not(a_cout et.distances.(d)) then Printf.printf "no way\n"
	  else (
            aux d;	  
	  ));
(*DynArray.iter (fun p->let (x,y)=p in print_int x;print_string "-";print_int y;print_newline()) path;*)
      DynArray.to_array path
	
end;;


