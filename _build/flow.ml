open Graph
open Tools

type flow_lbl = {
    flow: int;
    capacity: int
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
		| _ -> raise (Failure "string does not match \"flow/capacity\"")

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


(* Path *)
type path = diff_lbl out_arcs


let rec print_path path = match path with
	| [] -> Printf.printf "\n%!"
	| (id, _) :: rest -> Printf.printf "%d " id; print_path rest


(* Parcours en profondeur *)
(* Indique si un arc (son sommet destination) est marqué *)
let is_marked arc mark_list =
	match arc with
		| (id, _) -> List.mem id mark_list

let mark id mark_list = id :: mark_list

(* Enpiler un arc (son sommet destination) *)
let push pile arc = arc :: pile

(* Dépiler un arc (som sommet destination) *)
let pop pile =
	match pile with
		| [] -> raise (Failure "empty pile")
		| arc :: rest -> rest

let read pile =
	match List.hd pile with
		| (id, _) -> id

let get_unmarked_neighbor gr id mark_list =
	let rec loop arcs mark_list =
		match arcs with
			| [] -> None
			| arc :: rest ->
				if is_marked arc mark_list
				then loop rest mark_list
				else Some arc
	in
	loop (out_arcs gr id) mark_list

let dfs gr id_src id_dst =
	let rec loop gr id_dst pile mark_list =
		match pile with
			| [] -> []
			| _ ->
				begin
					let id = read pile in
					let new_mark_list = mark id mark_list in
					match get_unmarked_neighbor gr id mark_list with
						| Some (id, dlbl) when id = id_dst -> List.rev (push pile (id, dlbl))
						| Some arc -> loop gr id_dst (push pile arc) new_mark_list
						| None -> loop gr id_dst (pop pile) new_mark_list
				end
	in
	loop gr id_dst [(id_src, 0)] []

(* Minimum path *)
let get_min_cost_path path =
	match path with
		| [] -> raise (Failure "empty path")
		| [_] -> raise (Failure "the source is the destination")
		| [_ ; (_, dlbl)] -> dlbl
		| _ :: (_, dlbl) :: other_arcs ->
			let rec loop min arcs = 
				match arcs with
					| [] -> min
					| (_, cost) :: rest ->
						if cost < min then
							loop cost rest
						else
							loop min rest
			in
			loop dlbl other_arcs

let add_flow flbl n = {
	flow = flbl.flow + n;
	capacity = flbl.capacity
}

let substract_flow flbl n = {
	flow = flbl.flow - n;
	capacity = flbl.capacity
}

let update_flow_arc path n gr id1 id2 flbl =
	let rec loop arcs = match arcs with
		| (pid1, _) :: (pid2, _) :: _ when (pid1 = id1) && (pid2 = id2) -> new_arc gr id1 id2 (add_flow flbl n)
		| (pid1, _) :: (pid2, _) :: _ when (pid1 = id2) && (pid2 = id1) -> new_arc gr id1 id2 (substract_flow flbl n)
		| _ :: rest -> loop rest
		| _ -> gr
	in
	loop path

let update_flow_graph gr path =
	let n = get_min_cost_path path in
	e_fold gr (update_flow_arc path n) gr

let ford_fulkerson gr id_src id_dst =
	let rec loop fgr = 
		let dgr = generate_diff_gr fgr in
		let path = (dfs dgr id_src id_dst) in
		match path with
			| [] -> fgr
			| path -> loop (update_flow_graph fgr path)
	in
	loop gr
