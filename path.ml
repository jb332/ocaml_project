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

  

  

        
