open Graph
open Flow
open Tools
open Path


let rec optimize_flow gr =

  let diff_gr = generate_diff_gr gr.gr in

  let path_diff = find_path gr.src gr.dst diff_gr in
    if (List.length path_diff.lst > 0) then gr 

    else
      let min_flow = if(List.length path_diff.lst > 0) then min_cost path_diff else 0 in

      let rec new_flow_graph gr lst current_id = match lst with
        |[] -> gr
        |(id, lbl)::rest ->  let arc_a_modif = match (find_arc gr current_id id)
                               with
                                 |None -> failwith "Pas normal"
                                 |Some(a) -> a
            in
              new_flow_graph (new_arc gr current_id id ({flow = arc_a_modif.flow+min_flow; capacity = arc_a_modif.capacity})) rest id
      in
        optimize_flow ({src = gr.src;
                        dst = gr.dst;
                        gr = (new_flow_graph gr.gr path_diff.lst path_diff.dpt)})









