(* Calcul la puissance d'un entier *)
let rec puissance x n = match n with
| 0 -> 1
| _ -> x * (puissance x (n-1))
;;

(* Calcul la distance entre deux coordonn�es *)
let distance = fun coord1 coord2 ->
    let (x1, y1) = coord1 in
    let (x2, y2) = coord2 in 
    sqrt ( float_of_int(puissance (x2-x1) 2 + puissance (y2-y1) 2 ));;

(* D�finition de la vitesse minimale *)
let vitessemin = fun x ->
    x;;

(* D�finition de la vitesse maximale *)
let vitessemax = fun x ->
    x;;
   
(* D�finition du coefficient de l'abcisse de la droite angle = f(vitesse) *) 
let coef = fun minvit minang maxvit maxang ->
    let pi = 4.0 *. atan 1.0 in 
    let convert = pi /. 180. in 
    let coefficient = (minang -. maxang )*.convert /. (maxvit -. minvit) in
    let ordonnee = minang*.convert -. coefficient *. maxvit in
    (coefficient, ordonnee);;
   
(* Savoir le signe *)
let signe = fun x ->
    if x >=0 then
       1.
    else 
       -1.;;

(* Angle entre deux vecteurs *)
let angle = fun coord vitesse ->
    let (x, y) = coord in
    let (vx, vy) = vitesse in 
    let long = distance coord vitesse in
    signe(y)*.(acos(float_of_int(vx-x)/.long));;

(* Coordonn�e possible si on acc�l�re *)
let accelere = fun coord vitesse ->
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angle coord vitesse in
    let xs = int_of_float ((long +. 4.) *. cos(ang)) + x  in
    let ys = int_of_float ((long +. 4.) *. sin (ang)) + y in
    (xs, ys);;
   
(* Coordonn�e possible si on d�c�l�re *) 
let decelere = fun coord vitesse ->
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angle coord vitesse in
    let xs = int_of_float ((long -. 4.) *. cos ang) + x in
    let ys = int_of_float ((long -. 4.) *. sin ang) + y in
    (xs, ys);;
 
(* Coordonn�e possible si on tourne � gauche *)   
let gauche = fun coord vitesse minvit minang maxvit maxang-> 
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angle coord vitesse in
    let (coefficient, ordonnee) = coef minvit minang maxvit maxang in 
    let supang = coefficient *. long +. ordonnee in
    let xs = int_of_float (long *. cos(ang+. supang) ) + x in
    let ys = int_of_float (long *. sin(ang+. supang) )+ y in
    (xs, ys);;
    
(* Coordonn�e possible si on tourne � droite *)
let droite = fun coord vitesse minvit minang maxvit maxang->
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angle coord vitesse in
    let (coefficient, ordonnee) = coef minvit minang maxvit maxang in 
    let supang = coefficient *. long +. ordonnee in
    let xs = int_of_float ( long *. cos(ang -. supang) )+ x in 
    let ys = int_of_float ( long *. sin(ang -. supang) )+ y in
    (xs, ys);;
 
(* Possibilit�s dans un tableau *)   
let possibilites = fun coord vitesse minvit minang maxvit maxang -> 
    let suivante = [|(0,0);(0,0);(0,0);(0,0);(0,0)|] in 
    let (vx, vy) = vitesse in 
    let (ax, ay) = accelere coord vitesse in 
    let (dx, dy) = decelere coord vitesse in
    let (gx, gy) = gauche coord vitesse minvit minang maxvit maxang in
    let (drx, dry) = droite coord vitesse minvit minang maxvit maxang in
    suivante.(0) <- (vx, vy);
    suivante.(1) <- (ax, ay);
    suivante.(2) <- (dx, dy);
    suivante.(3) <- (gx, gy);
    suivante.(4) <- (drx, dry);
    suivante;;

(* D�finition d'un type noeud *)
type defnoeud = {mutable cout_g : float; mutable cout_h : float; mutable cout_f : float; mutable parent : int*int; mutable coord : int*int};;
type noeud = Noeud of defnoeud;;

let a={cout_g = 3.3; cout_h = 2.; cout_f = 3.; parent =(2,3); coord =(4,5)};;
a.cout_g <- 2.;;

(* Retirer un noeud d'une liste de noeud *)
let rec remove = fun noeud liste -> match liste with 
    [] -> failwith "Nan mais franchement tu d�connes, c'est pas comme �a qu'on fait les choses" 
   |Noeud h::q-> if h = noeud then 
             q
          else 
             Noeud h::(remove noeud q);;
  
(* Avoir la valeur du noeud qui se trouve � l'index 'num' *)           
let rec valeur = fun liste num -> match liste with 
    []-> failwith "y'a pas moyen djadja"
   |Noeud h::q-> if num = 0 then
               h
          else 
               valeur q (num-1);;
               
(* Valeur dans liste *)
let rec appartient = fun noeud liste -> match liste with
   []-> false
   |Noeud h::q -> if h== noeud then
                    true
                 else 
                    appartient noeud q;;                 

               
let essai = fun a ->
   let liste = ref [] in
   while !liste = [] do
      liste := List.append !liste [a]
   done;
   !liste;;

   
(* Algorithme A* *) 
let aetoile = fun noeuddepart noeudfinal->
   (*Initialisation des listes : ouverte et ferm�e, et variables *)
   let listeouverte = ref [] in
   let listefermee = ref [] in 
   let noeudcourant = ref {cout_g = 0.; cout_h = 0; cout_f = 0; parent  = noeuddepart; coord = noeuddepart} in
   let noeudobserve = ref {cout_g = 0.; cout_h = 0; cout_f = 0; parent  = noeuddepart; coord = noeuddepart} in
   let indexcourant = ref 0 in
   let solution = ref [] in 
   (*Initialisation du noeud courant et ajout � la liste ouverte *)
   let noeuddebut = {cout_g = 0.; cout_h = distance noeuddepart noeudfinal; cout_f = distance noeuddepart noeudfinal; parent  = noeuddepart; coord = noeuddepart} in 
   listeouverte := List.append !listeouverte [Noeud noeuddebut];
   (*Boucle jusqu'� trouver le noeud final*)
   while !listeouverte <> [] do 
      (*Initialisation du noeud courant au noeud du d�but*)
      noeudcourant := valeur !listeouverte 0;
      indexcourant := 0;
      for k=0 to List.length !listeouverte do 
          noeudobserve := valeur !listeouverte k;
          if !noeudobserve.cout_f < !noeudcourant.cout_f then 
              noeudcourant := !noeudobserve;
              indexcourant := k 
      done;
      (*Retire le noeud courant de la liste ouverte et l'ajoute � la liste ferm�e*)
      listeouverte := remove noeudcourant !listeouverte;
      listefermee := List.append !listefermee [Noeud !noeudcourant];
      (*Trouver la fin*)
      if noeudcourant := noeudfinal then
         let path = ref [] in
         let courant = ref noeudcourant in 
         while courant <> None do 
              path := List.append !path courant.coord;
              courant := !courant.parent
         done;
         solution := List.reverse path (*return*)
         break
      (*G�n�rer les enfants*)
      let enfants = ref [] in
      let poss = possibilites ... in
      for k=0 to 4 do 
         (*Avoir la position du noeud*)
         let pos = poss.(k) in 
         (*Sors pas du terrain*)
         if ... continue 
         (*Creer un nouveau noeud*)
         let nouveaunoeud =  ref {cout_g =0; cout_h=0; cout_f=0; parent=!noeudcourant; coord= pos}in
         (*append*)
         enfants := List.append !enfants [nouveaunoeud]
      done;
      for k=0 to List.length !enfants do
          let enfant = valeur !enfants k in
          (* enfant est dans la liste ferm�e*)
          if appartient enfant listefermee then 
             continue
          (* cr�er les valeurs de f. g. et h.*)
          enfant.cout_g <- !noeudcourant.cout_g +. 1.;
          enfant.cout_h <- distance enfant.coord noeudfinal;
          enfant.cout_f <- enfant.cout_g +. enfant.cout_f;
          (* enfant est d�j� dans la liste ouverte *)
          for k=0 to List.length !listeouverte do  
             let valnoeud = valeur !listeouverte k in
             if enfant = valnoeud && enfant.cout_g > valnoeud.cout_g then
                continue
          done;
          (* ajouter enfant � liste ouverte*)
          listeouverte := List.append !listeouverte [enfant]
      done;
      solution;;
                
          
         
         
         
      
      
      
https://khayyam.developpez.com/articles/algo/astar/
https://codes-sources.commentcamarche.net/source/54225-mise-en-evidence-de-l-algorithme-a-star-graphiquement
   
   
  
   
    
  
