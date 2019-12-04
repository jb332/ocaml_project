open Graph


let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (fun gr_acu id1 id2 lbl -> new_arc gr_acu id1 id2 (f lbl)) (clone_nodes gr)

let add_arc g id1 id2 n =
	let found_arc_lbl = find_arc g id1 id2 in
	begin
		match found_arc_lbl with
			| None -> new_arc g id1 id2 n
			| Some first_n -> new_arc g id1 id2 (first_n+n)
	end
