open Graph
open Tools

type flow_lbl = {
    flow: int;
    capacity: int
  }

type diff_lbl = int

type flow_graph = {
  src: int;
  dst: int;
  gr: flow_lbl graph
}
  


val flow_of_string: string -> flow_lbl
val string_of_flow: flow_lbl -> string

val diff_of_string: string -> diff_lbl
val string_of_diff: diff_lbl -> string

val generate_diff_gr: flow_lbl graph -> diff_lbl graph




    
  
