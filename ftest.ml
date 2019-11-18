open Gfile
open Tools
open Flow
open Graph
open Path

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
  (* Pour test graphs de flots 
  let fgraph = gmap fgraph_str flow_of_string in
  let dgraph = generate_diff_gr fgraph in
let dgraph_str = gmap dgraph string_of_diff in
  *)

  let fgraph = gmap fgraph_str int_of_string in
  let path = find_path 0 7 fgraph in


  

  p_iter (fun (a,b) -> Printf.printf "Noeud traversé : %d \n %!" a) path;
  Printf.printf "arc au cout min %d \n %! " (min_cost path) ;
    export "fgraph2_dot.gv" fgraph_str;
  (*export "dgraph_dot.gv" dgraph_str;*)

      (* Rewrite the graph that has been read. *)
      let () = write_file outfile (fgraph_str) in

      ()

