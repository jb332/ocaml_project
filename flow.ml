open Graph
open Tools

type flow_lbl = {
    flow: int;
    capacity: int
  }

type flow_graph = {
  src: int;
  dst: int;
  gr: flow_lbl graph
}

type diff_lbl = int

(* Conversions type string type flow (flot + capacite) *)
let flow_of_string str =
    let exploded_str = String.split_on_char '/' str in
    match exploded_str with
        | [fstr ; cstr] ->     
            {
                flow = (int_of_string fstr);
                capacity = (int_of_string cstr)
            }

let string_of_flow flbl = (string_of_int flbl.flow) ^ "/" ^ (string_of_int flbl.capacity) 

(* Conversions type string type diff (ecart) *)
let diff_of_string str = int_of_string str

let string_of_diff dlbl = string_of_int dlbl


(* Création d'un arc d'écart positif *)
let get_ecart_pos flbl = (flbl.capacity - flbl.flow)
let generate_diff_arc_pos gr_acu id1 id2 flbl =
	let ecart_pos = get_ecart_pos flbl in
	match ecart_pos with
		| 0 -> gr_acu
		| _ -> new_arc gr_acu id1 id2 ecart_pos

(* Création d'un arc d'écart négatif *)
let get_ecart_neg flbl = flbl.flow
let generate_diff_arc_neg gr_acu id1 id2 flbl =
	let ecart_neg = get_ecart_neg flbl in
	match ecart_neg with
		| 0 -> gr_acu
		| _ -> new_arc gr_acu id2 id1 ecart_neg

(* Génération d'un graphe d'écart à partir d'un graph de flots *)
let generate_diff_gr fgr =
    let dgr_nodes = clone_nodes fgr in
    let dgr_arcs_pos = e_fold fgr generate_diff_arc_pos dgr_nodes in
    let dgr_arcs_neg = e_fold fgr generate_diff_arc_neg dgr_arcs_pos in
    dgr_arcs_neg


                    


