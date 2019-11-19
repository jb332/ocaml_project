open Gfile
open Tools
open Flow
open Graph
open Path
open Folks_fulgerson

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

  (* Open file *)
  let fgraph_str = from_file infile in

  let fgraph = gmap fgraph_str flow_of_string in
  let dgraph = generate_diff_gr fgraph in
  let path = find_path 0 5 dgraph in
  p_iter (fun (a,b) -> Printf.printf "Noeud traversé : %d \n %!" a) path;
  Printf.printf "minimum du chemin %d" (min_cost path);
  
  
  let dgraph_str = gmap dgraph string_of_int in
  let d_optimized = optimize_flow ({src = 0; dst=5; gr = fgraph}) in
  let final_graph_str = gmap d_optimized.gr string_of_flow in
    (*let dgraph_str = gmap dgraph string_of_diff in *)


    (* let fgraph = gmap fgraph_str int_of_string in *)
    



    (* Test classe path 
       p_iter (fun (a,b) -> Printf.printf "Noeud traversé : %d \n %!" a) path;
       Printf.printf "arc au cout min %d \n %! " (min_cost path) ;
       export "fgraph2_dot.gv" fgraph_str;
    *)
  export "graph_flow_test.gv" fgraph_str ;
  export "dgraph_str.gv" dgraph_str;
    export "final_graph_dot.gv" final_graph_str; 

    (* Rewrite the graph that has been read. *)

    let () = write_file outfile (fgraph_str) in

      ()

