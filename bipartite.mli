type applicant = string
type applicants = applicant list

type job = string
type jobs = job list

type applicant_wishes = {
	applicant: applicant;
	wished_jobs: jobs
}

type applicants_wishes = applicant_wishes list


val solve_bipartite: applicants_wishes -> unit

