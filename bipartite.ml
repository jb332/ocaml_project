open Graph

type applicant = string
type job = string
type jobs = job list

type applicant_wishes = {
	applicant: applicant;
	wished_jobs: jobs
}

type applicants_whishes = applicant_wishes list

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

let associate_ids aw =
	let rec add_applicants n applicants = match applicants with
		| [] -> []
		| applicant :: rest -> (applicant, n) :: add_applicants (n+1) rest
	in
	let rec add_jobs n jobs = match jobs with
		| [] -> []
		| job :: rest -> (job, n) :: add_jobs (n+1) rest
	in
    let app_str_id_list = add_applicants 1 (get_applicants aw) in
    let nb_applicants = List.length app_str_id_list in
    let job_str_id_list = add_jobs (nb_applicants+1) (get_jobs aw) in
    let nb_jobs = List.length job_str_id_list in
    List.concat [ [("Source", 0)] ; app_str_id_list ; job_str_id_list ; [("Sink", nb_applicants+nb_jobs+1)] ]

let get_id_of_str cor_table str = List.assoc str cor_table

let rec print_cor_table cor_table = match cor_table with
    | [] -> Printf.printf "\n\n%!"
    | (str, id) :: rest -> Printf.printf "\n%s -> %d%!" str id; print_cor_table rest
    

let main =
    let aw = [
        {
            applicant = "John";
            wished_jobs = ["Carpenter"; "Mason"]
        } ; {
            applicant = "Joe";
            wished_jobs = ["Farmer" ; "Baker" ; "Carpenter"]
        } ; {
            applicant = "James";
            wished_jobs = ["Programmer" ; "Farmer" ; "Plumber"]
        }
    ] in
    let cor_table = associate_ids aw in
    print_cor_table cor_table

(*
let generate_nodes aw =
	let gr = new_node empty_graph 0 in
	let rec loop gr aw = match aw with
		| [] -> gr
		| x :: rest -> loop (new_node gr x.applicant) rest
	in
*)	



let ignore_spaces str = String.concat "" (String.split_on_char ' ' str)
