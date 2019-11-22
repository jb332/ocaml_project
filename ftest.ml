open Gfile
open Tools
open Flow
open Graph

(* Run something like "./ftest.native graph 0 5 new_graph" *)

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  let graph_str = from_file infile in
  let graph = generate_flow_graph graph_str in
  let new_graph = ford_fulkerson graph _source _sink in

  let new_graph_str = gmap new_graph string_of_flow in

  export "graph_dot.gv" graph_str;
  export outfile new_graph_str;

  (*
  dot -Tsvg graph_dot.gv > graph.svg && dot -Tsvg new_graph_dot.gv > new_graph.svg
  *)

  let () = write_file outfile new_graph_str in

  ()

  (*
  (* Open file *)
  let fgraph_str = from_file infile in
  let fgraph = gmap fgraph_str flow_of_string in
  let dgraph = generate_diff_gr fgraph in
  let dgraph_str = gmap dgraph string_of_diff in
  let path = dfs dgraph 1 5 in
  print_path path;
  let new_fgraph = update_flow_graph fgraph path in
  let new_fgraph_str = gmap new_fgraph string_of_flow in
  
  export "new_fgraph_dot.gv" new_fgraph_str;
  export "fgraph_dot.gv" fgraph_str;
  export "dgraph_dot.gv" dgraph_str;
  (*
  bash command to generate images
  dot -Tsvg fgraph_dot.gv > graph_flow.svg && dot -Tsvg dgraph_dot.gv > graph_diff.svg && dot -Tsvg new_fgraph_dot.gv > new_graph_flow.svg
  *)
  
  (* Rewrite the graph that has been read. *)
  let () = write_file outfile (dgraph_str) in
  ()
  *)
