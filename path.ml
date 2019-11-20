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

let padd_arc arc path = arc :: path.lst

let p_iter fct path = List.iter fct path.lst



let noeuds_blancs = n_fold gr (fun el lst ->
    {
      etat = Blanc;


      noeud = el
    }
    
    :: lst) []
in

(* Soit les appels recursifs renvoient le path, soit il est construit au fur et à mesure
   ne pas utiliser iter *)
let find_path ids idd gr =
      
  let rec loop ids idd gr lst path=
    let traiter_noeud node path=
    match node with
      |(id, lbl) ->
        if (id = idd) then add_arc (id, lbl) path
      else
        loop ids idd gr 
     (out_arcs gr ids)

  

<<<<<<< HEAD
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

let min_cost path =
  let rec find_min lst min = match lst with
    |[] -> min
    |(id, lbl)::b -> if (lbl<min) then find_min b lbl else find_min b min
  in

  let (frst_id, frst_lbl) = List.hd path.lst in
    find_min path.lst frst_lbl
(*    
with
      |Failure s -> Printf.printf "Acces a la tete d'une liste vide %s" s; *)



let rec contains id path = match path.lst with
  |[] -> false
  |(i, _)::rest -> if(i = id) then true else contains id {dpt=path.dpt; lst = rest}






=======
  
>>>>>>> origin/alt

        
