open Graph
open Flow

type applicant = string
type applicants = applicant list

type job = string
type jobs = job list

type applicant_wishes = {
	applicant: applicant;
	wished_jobs: jobs
}

type applicants_wishes = applicant_wishes list

type id_string_entry = (string * id)
type id_string_table = id_string_entry list

(* Create a function that extracts the file data to create an applicants_wishes object *)




(* Merge two lists and eliminates duplicatas *)
let rec or_on_lists l1 l2 = match l2 with
	| [] -> l1
	| job :: rest when List.mem job l1 -> or_on_lists l1 rest
	| job :: rest -> job :: or_on_lists l1 rest

(* Get the list of applicants out of an applicants_wishes object *)
let get_applicants aw = List.map (fun aw -> aw.applicant) aw

(* Get the list of jobs out of an applicants wishes object *)
let rec get_jobs aw = match aw with
	| [] -> []
	| {applicant; wished_jobs} :: rest -> or_on_lists wished_jobs (get_jobs rest)

(* Returns a correspondance table between a node id and the source, the names, the jobs, and the sink *)

(*
let add_elements lst_of_aw aw = List.fold_left (fun ((elem_str, elem_id) :: other_entries) elmeent -> ((element, elem_id + 1) :: (elem_str, elem_id) :: other_entries)) [("Source", 0)] (lst_of_aw aw)
let add_applicants = add_elements get_applicants
let add_jobs = add_elements get_jobs
*)

let associate_ids applicants jobs =
	let rec add_applicants n applicants = match applicants with
		| [] -> []
		| applicant :: rest -> (applicant, n) :: add_applicants (n+1) rest
	in
	let rec add_jobs n jobs = match jobs with
		| [] -> []
		| job :: rest -> (job, n) :: add_jobs (n+1) rest
	in
  let app_str_id_list = add_applicants 1 applicants in
  let nb_applicants = List.length app_str_id_list in
  let job_str_id_list = add_jobs (nb_applicants+1) jobs in
  let nb_jobs = List.length job_str_id_list in
  List.concat [ [("Source", 0)] ; app_str_id_list ; job_str_id_list ; [("Sink", nb_applicants+nb_jobs+1)] ]

let get_id_of_str cor_table str = List.assoc str cor_table

let get_str_of_id cor_table id =
	let element = List.find (fun (current_str, current_id) -> current_id = id) cor_table in
	match element with (str_found, id_found) -> str_found

(*List.assoc id (List.map (fun (str, id) -> (id, str)) cor_table)*)

let rec print_cor_table cor_table = match cor_table with
	| [] -> Printf.printf "\n\n%!"
	| (str, id) :: rest -> Printf.printf "\n%s -> %d%!" str id; print_cor_table rest

let generate_graph aw cor_table applicants jobs =
	let generate_nodes cor_table = 
		let nodes = List.map (fun (_, id) -> id) cor_table in
		List.fold_left new_node empty_graph nodes
	in
	let generate_applicant_arcs applicant_wishes graph =
		let applicant_id = get_id_of_str cor_table applicant_wishes.applicant in
		let applicant_wished_jobs_ids = List.map (get_id_of_str cor_table) applicant_wishes.wished_jobs in
		let applicant_arcs = List.map (fun applicant_wished_job_id -> (applicant_id, applicant_wished_job_id, {flow = 0 ; capacity = 1})) applicant_wished_jobs_ids in
		let rec loop applicant_arcs = match applicant_arcs with
			| [] -> graph
			| (id1, id2, flbl) :: rest -> new_arc (loop rest) id1 id2 flbl
		in
		loop applicant_arcs
	in
	let rec generate_arcs aw graph = match aw with
		| [] -> graph
		| applicant_wishes :: rest -> generate_arcs rest (generate_applicant_arcs applicant_wishes graph)
	in
	let rec add_source_arcs graph applicants = match applicants with
		| [] -> graph
		| applicant :: rest -> new_arc (add_source_arcs graph rest) 0 (get_id_of_str cor_table applicant) {flow = 0 ; capacity = 1}
	in
	let rec add_sink_arcs graph jobs = match jobs with
		| [] -> graph
		| job :: rest -> new_arc (add_sink_arcs graph rest) (get_id_of_str cor_table job) ((List.length cor_table)-1) {flow = 0 ; capacity = 1}
	in
	add_source_arcs (add_sink_arcs (generate_arcs aw (generate_nodes cor_table)) jobs) applicants

let print_affectations graph_out cor_table =
	let applicant_nodes = List.map (fun (id, _) -> id) (out_arcs graph_out 0) in
	let get_attributed_job_node applicant_node =
		match List.find (fun (id, flbl) -> flbl.flow = 1) (out_arcs graph_out applicant_node) with
			| (id, _) -> id
	in
	let print_affectation applicant_node = 
		try
			Printf.printf "%s : %s\n%!" (get_str_of_id cor_table applicant_node) (get_str_of_id cor_table (get_attributed_job_node applicant_node))
		with
			Not_found -> Printf.printf "%s : -\n%!" (get_str_of_id cor_table applicant_node)
	in
	List.iter print_affectation applicant_nodes

let solve_bipartite aw =
	let applicants = get_applicants aw in
	let jobs = get_jobs aw in
	let cor_table = associate_ids applicants jobs in
	let graph_in = generate_graph aw cor_table applicants jobs in
	let graph_out = ford_fulkerson graph_in 0 ((List.length cor_table)-1) false in
	print_affectations graph_out cor_table

let test_bipartite =
	let aw = [
		{
			applicant = "John";
			wished_jobs = ["Carpenter"; "Mason"]
		} ; {
			applicant = "Joe";
			wished_jobs = ["Farmer" ; "Baker" ; "Carpenter"]
		} ; {
			applicant = "Jack";
			wished_jobs = ["Carpenter"; "Mason"]
		} ; {
			applicant = "Jake";
			wished_jobs = ["Carpenter"; "Mason"]
		} ; {
			applicant = "James";
			wished_jobs = ["Programmer" ; "Farmer" ; "Plumber"]
		}
	] in
	solve_bipartite aw

