open Graph
open Tools

type flow_lbl
type diff_lbl


val flow_of_string: string -> flow_lbl
val string_of_flow: flow_lbl -> string

val diff_of_string: string -> diff_lbl
val string_of_diff: diff_lbl -> string

val generate_diff_gr: flow_lbl graph -> diff_lbl graph
