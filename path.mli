open Graph

(*Liste de Noeuds*)
type 'a path

val init_path: int -> 'a path

val add_arc: (id * 'a) -> 'a path -> 'a path

val p_iter: 'a path -> unit

val find_path: id -> id -> 'a graph -> 'a path

    
