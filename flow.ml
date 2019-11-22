open Graph
open Tools

(* Label types *)
type flow_lbl = {
    flow: int;
    capacity: int
}

type diff_lbl = int

(* Path type *)
type path = diff_lbl out_arcs


(*************************
**** Difference graph ****
*************************)

(* Conversion functions between label types and type string *)
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

let diff_of_string str = int_of_string str

let string_of_diff dlbl = string_of_int dlbl

(* Generate a flow graph from a string graph having capacities only (flow is set to 0) *)
let generate_flow_graph gr = gmap gr (fun i -> {flow = 0;
                                                capacity = int_of_string i})

(* Create a positive difference bow *)
let get_ecart_pos flbl = (flbl.capacity - flbl.flow)
let generate_diff_arc_pos gr_acu id1 id2 flbl =
	let ecart_pos = get_ecart_pos flbl in
	match ecart_pos with
		| 0 -> gr_acu
		| _ -> new_arc gr_acu id1 id2 ecart_pos

(* Create a negative difference bow *)
let get_ecart_neg flbl = flbl.flow
let generate_diff_arc_neg gr_acu id1 id2 flbl =
	let ecart_neg = get_ecart_neg flbl in
	match ecart_neg with
		| 0 -> gr_acu
		| _ -> new_arc gr_acu id2 id1 ecart_neg

(* Generate a difference graph from a flow graph *)
let generate_diff_gr fgr =
    let dgr_nodes = clone_nodes fgr in
    let dgr_arcs_pos = e_fold fgr generate_diff_arc_pos dgr_nodes in
    let dgr_arcs_neg = e_fold fgr generate_diff_arc_neg dgr_arcs_pos in
    dgr_arcs_neg


(**************************************
**** Depth-First Search Algorithm *****
**************************************)

(* Returns true if a vertex is marked *)
let is_marked arc mark_list =
	match arc with
		| (id, _) -> List.mem id mark_list

(* Marks a vertex *)
let mark id mark_list = id :: mark_list

(* Enpiler un arc (son sommet destination) *)
let push pile arc = arc :: pile

(* Pop the stack *)
(* To be used in the dfs algortihm or in the functions it calls *)
let pop pile =
	match pile with
		| [] -> raise (Failure "empty pile")
		| arc :: rest -> rest

let read pile =
	match List.hd pile with
		| (id, _) -> id

(* Get all unmarked out arcs of a node *)
(* To be used in the following dfs algorithm *)
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

(* Depth-first search algorithm *)
let dfs gr id_src id_dst =
	let rec loop gr id_dst pile mark_list =
		match pile with
			| [] -> []
			| _ ->
				begin
					let id = read pile in
					let new_mark_list = mark id mark_list in
					match get_unmarked_neighbor gr id new_mark_list with
						| Some (id, dlbl) when id = id_dst -> List.rev (push pile (id, dlbl))
						| Some arc -> loop gr id_dst (push pile arc) new_mark_list
						| None -> loop gr id_dst (pop pile) new_mark_list
				end
	in
	loop gr id_dst [(id_src, 0)] []

(* Print a path *)
let print_path path = match path with
	| [] -> Printf.printf "aucun chemin\n%!"
	| [_] -> Printf.printf "source = destination\n%!"
	| (id_src, _) :: path_rest ->
		let rec loop arcs = match arcs with
			| [] -> Printf.printf "\n%!"
			| (id, _) :: rest -> Printf.printf " -> %d%!" id; loop rest
		in
		Printf.printf "chemin : %d%!" id_src;
		loop path_rest


(*********************************
**** Ford Fulkerson algorithm ****
*********************************)

(* Get the minimum flow of the arcs in the path *)
(* To be used in "update_flow_graph" *)
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

(* Increment flow by n *)
let add_flow flbl n = {
	flow = flbl.flow + n;
	capacity = flbl.capacity
}

(* Decrement flow by n *)
let substract_flow flbl n = {
	flow = flbl.flow - n;
	capacity = flbl.capacity
}

(* Check if the current arc is in the path and increment or decrement it with n if it is *)
(* To be used with "e_fold" in the "update_flow_graph" function *)
let update_flow_arc path n gr id1 id2 flbl =
	let rec loop arcs = match arcs with
		| (pid1, _) :: (pid2, _) :: _ when (pid1 = id1) && (pid2 = id2) -> new_arc gr id1 id2 (add_flow flbl n)
		| (pid1, _) :: (pid2, _) :: _ when (pid1 = id2) && (pid2 = id1) -> new_arc gr id1 id2 (substract_flow flbl n)
		| _ :: rest -> loop rest
		| _ -> gr
	in
	loop path

(* Update the graph by incrementing or decrementing the flow of the arcs in the path *)
(* To be used in "ford_fulkerson" *)
let update_flow_graph gr path =
	let n = get_min_cost_path path in
	(* start print zone *)
	Printf.printf "\n%!";
	print_path path;
	Printf.printf "flot minimum : %d\n%!" n;
	(* end print zone *)
	e_fold gr (update_flow_arc path n) gr

(* Get the sum of the flows going out of the source *)
(* To be used in "ford_fulkerson" on the max flow graph to get the max flow *)
let flow_src_out gr id_src =
	let rec loop out_arcs = match out_arcs with
		| [] -> 0
		| (_, flbl) :: rest -> flbl.flow + loop rest
	in
	loop (out_arcs gr id_src)

(* Max flow algorithm *)
let ford_fulkerson gr id_src id_dst =
	let rec loop fgr = 
		let dgr = generate_diff_gr fgr in
		let path = (dfs dgr id_src id_dst) in
		match path with
			| [] -> fgr
			| path -> loop (update_flow_graph fgr path)
	in
	let max_flow_gr = loop gr in
	Printf.printf "\nmax flow : %d\n%!" (flow_src_out max_flow_gr id_src);
	max_flow_gr
