open Gfile
open Tools
open Flow
open Graph
open Bipartite

let () =

	(*****************************)
	(**** TEST FORD-FULKERSON ****)
	(*****************************)

	(* Minimal project (commented by default) *)
	(* In order to test, run something like "./ftest.native graph1 0 5 new_graph_dot.gv"	*)
	(* You can test our ford-fulkerson algorithm on several graph files (graph1, graph2 and graph3) *)

	(*
	(* Check the number of command-line arguments *)
	if Array.length Sys.argv <> 5 then
		begin
			Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
			exit 0
		end ;


	(* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
	let infile = Sys.argv.(1)
	and outfile = Sys.argv.(4)
	and _source = int_of_string Sys.argv.(2)
	and _sink = int_of_string Sys.argv.(3)
	in

	let graph_str = from_file infile in
	let graph = generate_flow_graph graph_str in
	let fgraph_str = gmap graph string_of_flow in
	let new_graph = ford_fulkerson graph _source _sink true in 
	let new_graph_str = gmap new_graph string_of_flow in

	export "graph_dot.gv" fgraph_str;
	export outfile new_graph_str;
	
	(*
	This is the command to convert ".gv" files into ".svg" vectorial images :
	"dot -Tsvg graph_dot.gv > graph.svg && dot -Tsvg new_graph_dot.gv > new_graph.svg"
	*)

	()
	*)



	(*******************************************)
	(**** TEST BIPARTITE EMPLOYMENT PROBLEM ****)
	(*******************************************)


	(* BIPARTITE EMPLOYMENT PROBLEM *)
	(* Medium project : uncommented by default *)
	(* In order to test, run something like "./ftest.native candidates1" *)
	(* You can test the problem solving on several problem files (applicants1, applicants2) *)

	if Array.length Sys.argv <> 2 then
		begin
			Printf.printf "\nUsage: %s infile\n\n%!" Sys.argv.(0) ;
			exit 0
		end ;

	let infile = Sys.argv.(1) in
	let aw = from_file_candidate infile in
	solve_bipartite aw

