open Graph

(*Liste de Noeuds*)
type 'a path

val init_path: int -> 'a path

val padd_arc: (id * 'a) -> 'a path -> 'a path

val p_iter: (id * 'a -> unit) -> 'a path -> unit

val find_path: id -> id -> 'a graph -> 'a path

val min_cost: 'a path -> 'a

