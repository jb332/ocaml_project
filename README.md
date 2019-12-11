# ocaml_project

Compilation du projet : ocamlbuild ftest.ml

Version minimale et version moyenne implémentées. 
La version moyenne correspond à la résolution d'un problème mettant en oeuvre des graphes bipartites. 
L'exemple choisi est l'affectation de M postes à N postulants.

C'est le projet testé par défaut : pour lancer le test : ./ftest.byte fichier_candidat

Le fichier candidat a pour format : 
candidat1 : qualification1,qualification2...,qualificationn 
candidat2 : qualification1,qualification2,...,qualificationn
Le projet contient 2 fichiers candidats : candidate1 et candidate2.

Pour tester la version minimale, commenter le test bipartite et décommenter test ford-fulkerson.
Commande de test : ./ftest.byte graph_src source puit graph_resultat
Pour générer le fichier svg du graphe
dot -Tsvg graph_dot.gv > graph.svg && dot -Tsvg new_graph_dot.gv > new_graph.svg" (graphe initial)

La version better project (algo max-flow min cost) n'a pas été réalisée
