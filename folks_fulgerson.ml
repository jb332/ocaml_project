open Graph
open Flow
open Tools
    

let rec optimize_flow gr =

  let diff_gr = generate_diff_gr gr.gr in

  let path_diff = find_path gr.src gr.dst diff_gr in
  let min_flow = if(List.length path_diff.lst > 0) then min_cost path_diff else 0

 let rec new_flow_graph gr lst current_id = match lst with
  |[] -> gr
  |(id, lbl)::rest ->  add_arc
       
    


  
