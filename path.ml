(*DANS LE PROCHAIN EPISODE ::: 
  1- Implementer le modulte path (type record : un noeud depart -> une liste de noeuds)
  2- Methode de parcours en largeur qui renvoie un chemin de s vers p
  3- Type record contenant flot et capacite 
  4- Deroulement de l'algorithme ... *)


open Graph

type 'a path = {
  dpt: int;

  lst: (id * 'a) list
}

let init_path id = {
  dpt = id;

  lst = []

}

let padd_arc arc path = {

  dpt = path.dpt;

  lst = arc :: path.lst

}

let p_iter fct path = List.iter fct path.lst





(* Soit les appels recursifs renvoient le path, soit il est construit au fur et à mesure
   ne pas utiliser iter *)
let find_path ids idd gr =


  let rec dfs ids idd gr lst_parc lst_en_cours=
    let arcs_sortants = out_arcs gr ids in
    let rec traiter_arcs arcs_sortants =
      match arcs_sortants with
        |[] -> ([], lst_parc)
        |(id, lbl)::b -> if (id = idd) then ((id, lbl)::lst_en_cours, id::lst_parc)
            else
            if List.mem id lst_parc  then traiter_arcs b else
              match dfs id idd gr (id::lst_parc) ((id, lbl)::lst_en_cours) with
                |([], lst_parc) -> traiter_arcs b
                |a-> a
    in
      traiter_arcs arcs_sortants
  in
    match dfs ids idd gr [] [] with
      |(pth, lst_parc) -> {dpt = ids; lst = (List.rev pth)}






