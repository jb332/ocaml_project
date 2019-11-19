open Graph
open Tools

type flow_lbl
type diff_lbl = int


val flow_of_string: string -> flow_lbl
val string_of_flow: flow_lbl -> string

val diff_of_string: string -> diff_lbl
val string_of_diff: diff_lbl -> string

val generate_diff_gr: flow_lbl graph -> diff_lbl graph



type path

val dfs: diff_lbl graph -> id -> id -> path
val print_path: path -> unit

val update_flow_graph: flow_lbl graph -> path -> flow_lbl graph

val ford_fulkerson: flow_lbl graph -> id -> id -> flow_lbl graph
