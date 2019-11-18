open Graph
open Flow
open Tools
    

let rec optimize_flow gr =

  let diff_gr = generate_diff_gr gr.gr in

  let path_diff = find_path gr.src gr.dst diff_gr in

  match path_diff.lst with
  |[] -> gr
  |a -> let flow_min = min_cost path_diff in
    gmap 
    


  
