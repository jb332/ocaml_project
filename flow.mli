open Graph
open Tools

type flow_lbl = {
    flow: int;
    capacity: int
}

val flow_of_string: string -> flow_lbl
val string_of_flow: flow_lbl -> string

val ford_fulkerson: flow_lbl graph -> id -> id -> bool -> flow_lbl graph

val generate_flow_graph: string graph -> flow_lbl graph 
